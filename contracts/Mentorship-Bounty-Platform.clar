(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-already-exists (err u104))
(define-constant err-invalid-status (err u105))
(define-constant err-insufficient-funds (err u106))

(define-data-var next-bounty-id uint u1)
(define-data-var platform-fee-rate uint u250)

(define-map bounties
  { bounty-id: uint }
  {
    creator: principal,
    mentor: (optional principal),
    mentee: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    amount: uint,
    status: (string-ascii 20),
    created-at: uint,
    completed-at: (optional uint)
  }
)

(define-map mentor-profiles
  { mentor: principal }
  {
    name: (string-ascii 50),
    expertise: (string-ascii 200),
    hourly-rate: uint,
    total-earnings: uint,
    completed-sessions: uint,
    rating: uint,
    active: bool
  }
)

(define-map mentee-profiles
  { mentee: principal }
  {
    name: (string-ascii 50),
    total-spent: uint,
    sessions-completed: uint,
    active: bool
  }
)

(define-map session-verifications
  { bounty-id: uint }
  {
    mentee-verified: bool,
    mentor-confirmed: bool,
    verification-deadline: uint,
    session-notes: (string-ascii 300)
  }
)

(define-map bounty-applications
  { bounty-id: uint, mentor: principal }
  {
    applied-at: uint,
    proposal: (string-ascii 300),
    status: (string-ascii 20)
  }
)

(define-public (create-mentor-profile (name (string-ascii 50)) (expertise (string-ascii 200)) (hourly-rate uint))
  (let ((mentor tx-sender))
    (asserts! (> hourly-rate u0) err-invalid-amount)
    (asserts! (is-none (map-get? mentor-profiles { mentor: mentor })) err-already-exists)
    (ok (map-set mentor-profiles
      { mentor: mentor }
      {
        name: name,
        expertise: expertise,
        hourly-rate: hourly-rate,
        total-earnings: u0,
        completed-sessions: u0,
        rating: u5,
        active: true
      }
    ))
  )
)

(define-public (create-mentee-profile (name (string-ascii 50)))
  (let ((mentee tx-sender))
    (asserts! (is-none (map-get? mentee-profiles { mentee: mentee })) err-already-exists)
    (ok (map-set mentee-profiles
      { mentee: mentee }
      {
        name: name,
        total-spent: u0,
        sessions-completed: u0,
        active: true
      }
    ))
  )
)

(define-public (create-bounty (title (string-ascii 100)) (description (string-ascii 500)) (amount uint))
  (let (
    (bounty-id (var-get next-bounty-id))
    (creator tx-sender)
  )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (is-some (map-get? mentee-profiles { mentee: creator })) err-unauthorized)
    (try! (stx-transfer? amount creator (as-contract tx-sender)))
    (map-set bounties
      { bounty-id: bounty-id }
      {
        creator: creator,
        mentor: none,
        mentee: creator,
        title: title,
        description: description,
        amount: amount,
        status: "open",
        created-at: stacks-block-height,
        completed-at: none
      }
    )
    (var-set next-bounty-id (+ bounty-id u1))
    (ok bounty-id)
  )
)

(define-public (apply-for-bounty (bounty-id uint) (proposal (string-ascii 300)))
  (let (
    (mentor tx-sender)
    (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
  )
    (asserts! (is-some (map-get? mentor-profiles { mentor: mentor })) err-unauthorized)
    (asserts! (is-eq (get status bounty) "open") err-invalid-status)
    (asserts! (is-none (map-get? bounty-applications { bounty-id: bounty-id, mentor: mentor })) err-already-exists)
    (ok (map-set bounty-applications
      { bounty-id: bounty-id, mentor: mentor }
      {
        applied-at: stacks-block-height,
        proposal: proposal,
        status: "pending"
      }
    ))
  )
)

(define-public (accept-mentor (bounty-id uint) (mentor principal))
  (let (
    (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
    (application (unwrap! (map-get? bounty-applications { bounty-id: bounty-id, mentor: mentor }) err-not-found))
  )
    (asserts! (is-eq tx-sender (get creator bounty)) err-unauthorized)
    (asserts! (is-eq (get status bounty) "open") err-invalid-status)
    (asserts! (is-eq (get status application) "pending") err-invalid-status)
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { mentor: (some mentor), status: "in-progress" })
    )
    (map-set bounty-applications
      { bounty-id: bounty-id, mentor: mentor }
      (merge application { status: "accepted" })
    )
    (map-set session-verifications
      { bounty-id: bounty-id }
      {
        mentee-verified: false,
        mentor-confirmed: false,
        verification-deadline: (+ stacks-block-height u144),
        session-notes: ""
      }
    )
    (ok true)
  )
)

(define-public (complete-session (bounty-id uint) (session-notes (string-ascii 300)))
  (let (
    (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
    (mentor (unwrap! (get mentor bounty) err-not-found))
    (verification (unwrap! (map-get? session-verifications { bounty-id: bounty-id }) err-not-found))
  )
    (asserts! (is-eq tx-sender mentor) err-unauthorized)
    (asserts! (is-eq (get status bounty) "in-progress") err-invalid-status)
    (map-set session-verifications
      { bounty-id: bounty-id }
      (merge verification { mentor-confirmed: true, session-notes: session-notes })
    )
    (ok true)
  )
)

(define-public (verify-session (bounty-id uint))
  (let (
    (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
    (verification (unwrap! (map-get? session-verifications { bounty-id: bounty-id }) err-not-found))
    (mentor (unwrap! (get mentor bounty) err-not-found))
    (mentee (get mentee bounty))
    (amount (get amount bounty))
    (platform-fee (/ (* amount (var-get platform-fee-rate)) u10000))
    (mentor-payment (- amount platform-fee))
  )
    (asserts! (is-eq tx-sender mentee) err-unauthorized)
    (asserts! (is-eq (get status bounty) "in-progress") err-invalid-status)
    (asserts! (get mentor-confirmed verification) err-invalid-status)
    (asserts! (< stacks-block-height (get verification-deadline verification)) err-invalid-status)
    
    (try! (as-contract (stx-transfer? mentor-payment tx-sender mentor)))
    (try! (as-contract (stx-transfer? platform-fee tx-sender contract-owner)))
    
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { status: "completed", completed-at: (some stacks-block-height) })
    )
    
    (map-set session-verifications
      { bounty-id: bounty-id }
      (merge verification { mentee-verified: true })
    )
    
    (let ((mentor-profile (unwrap! (map-get? mentor-profiles { mentor: mentor }) err-not-found)))
      (map-set mentor-profiles
        { mentor: mentor }
        (merge mentor-profile {
          total-earnings: (+ (get total-earnings mentor-profile) mentor-payment),
          completed-sessions: (+ (get completed-sessions mentor-profile) u1)
        })
      )
    )
    
    (let ((mentee-profile (unwrap! (map-get? mentee-profiles { mentee: mentee }) err-not-found)))
      (map-set mentee-profiles
        { mentee: mentee }
        (merge mentee-profile {
          total-spent: (+ (get total-spent mentee-profile) amount),
          sessions-completed: (+ (get sessions-completed mentee-profile) u1)
        })
      )
    )
    
    (ok true)
  )
)

(define-public (dispute-session (bounty-id uint))
  (let (
    (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
    (verification (unwrap! (map-get? session-verifications { bounty-id: bounty-id }) err-not-found))
    (mentee (get mentee bounty))
    (amount (get amount bounty))
  )
    (asserts! (is-eq tx-sender mentee) err-unauthorized)
    (asserts! (is-eq (get status bounty) "in-progress") err-invalid-status)
    (asserts! (> stacks-block-height (get verification-deadline verification)) err-invalid-status)
    
    (try! (as-contract (stx-transfer? amount tx-sender mentee)))
    
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { status: "disputed" })
    )
    
    (ok true)
  )
)

(define-public (cancel-bounty (bounty-id uint))
  (let (
    (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
    (creator (get creator bounty))
    (amount (get amount bounty))
  )
    (asserts! (is-eq tx-sender creator) err-unauthorized)
    (asserts! (is-eq (get status bounty) "open") err-invalid-status)
    
    (try! (as-contract (stx-transfer? amount tx-sender creator)))
    
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { status: "cancelled" })
    )
    
    (ok true)
  )
)

(define-public (update-platform-fee (new-fee-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee-rate u1000) err-invalid-amount)
    (ok (var-set platform-fee-rate new-fee-rate))
  )
)

(define-read-only (get-bounty (bounty-id uint))
  (map-get? bounties { bounty-id: bounty-id })
)

(define-read-only (get-mentor-profile (mentor principal))
  (map-get? mentor-profiles { mentor: mentor })
)

(define-read-only (get-mentee-profile (mentee principal))
  (map-get? mentee-profiles { mentee: mentee })
)

(define-read-only (get-session-verification (bounty-id uint))
  (map-get? session-verifications { bounty-id: bounty-id })
)

(define-read-only (get-bounty-application (bounty-id uint) (mentor principal))
  (map-get? bounty-applications { bounty-id: bounty-id, mentor: mentor })
)

(define-read-only (get-platform-fee-rate)
  (var-get platform-fee-rate)
)

(define-read-only (get-next-bounty-id)
  (var-get next-bounty-id)
)

(define-map mentor-achievements
  { mentor: principal }
  {
    sessions-milestone: uint,
    earnings-milestone: uint,
    consistency-streak: uint,
    excellence-badges: uint,
    mentor-of-month: uint,
    total-achievement-points: uint,
    last-achievement-at: uint
  }
)

(define-map achievement-rewards
  { mentor: principal, achievement-type: (string-ascii 20) }
  {
    awarded-at: uint,
    bonus-amount: uint,
    achievement-level: uint
  }
)

(define-data-var achievement-fund uint u0)
(define-data-var monthly-top-mentor (optional principal) none)
(define-data-var current-month-block uint u0)

(define-constant achievement-session-milestones (list u5 u10 u25 u50 u100))
(define-constant achievement-earnings-milestones (list u1000000000 u5000000000 u10000000000 u50000000000))
(define-constant achievement-bonuses (list u5000000 u10000000 u25000000 u50000000 u100000000))

(define-public (initialize-mentor-achievements (mentor principal))
  (begin
    (asserts! (is-some (map-get? mentor-profiles { mentor: mentor })) err-unauthorized)
    (asserts! (is-none (map-get? mentor-achievements { mentor: mentor })) err-already-exists)
    (ok (map-set mentor-achievements
      { mentor: mentor }
      {
        sessions-milestone: u0,
        earnings-milestone: u0,
        consistency-streak: u0,
        excellence-badges: u0,
        mentor-of-month: u0,
        total-achievement-points: u0,
        last-achievement-at: u0
      }
    ))
  )
)

(define-public (check-session-milestone (mentor principal))
  (let (
    (mentor-profile (unwrap! (map-get? mentor-profiles { mentor: mentor }) err-not-found))
    (achievements (unwrap! (map-get? mentor-achievements { mentor: mentor }) err-not-found))
    (current-sessions (get completed-sessions mentor-profile))
    (current-milestone (get sessions-milestone achievements))
  )
    (let ((next-milestone (+ current-milestone u1)))
      (if (and 
        (< current-milestone (len achievement-session-milestones))
        (>= current-sessions (unwrap-panic (element-at achievement-session-milestones current-milestone))))
        (begin
          (map-set mentor-achievements
            { mentor: mentor }
            (merge achievements {
              sessions-milestone: next-milestone,
              total-achievement-points: (+ (get total-achievement-points achievements) u10),
              last-achievement-at: stacks-block-height
            })
          )
          (try! (award-achievement-bonus mentor "session-milestone" next-milestone))
          (ok true)
        )
        (ok false)
      )
    )
  )
)

(define-public (check-earnings-milestone (mentor principal))
  (let (
    (mentor-profile (unwrap! (map-get? mentor-profiles { mentor: mentor }) err-not-found))
    (achievements (unwrap! (map-get? mentor-achievements { mentor: mentor }) err-not-found))
    (current-earnings (get total-earnings mentor-profile))
    (current-milestone (get earnings-milestone achievements))
  )
    (let ((next-milestone (+ current-milestone u1)))
      (if (and 
        (< current-milestone (len achievement-earnings-milestones))
        (>= current-earnings (unwrap-panic (element-at achievement-earnings-milestones current-milestone))))
        (begin
          (map-set mentor-achievements
            { mentor: mentor }
            (merge achievements {
              earnings-milestone: next-milestone,
              total-achievement-points: (+ (get total-achievement-points achievements) u20),
              last-achievement-at: stacks-block-height
            })
          )
          (try! (award-achievement-bonus mentor "earnings-milestone" next-milestone))
          (ok true)
        )
        (ok false)
      )
    )
  )
)

(define-private (award-achievement-bonus (mentor principal) (achievement-type (string-ascii 20)) (level uint))
  (let (
    (bonus-amount (if (< level (len achievement-bonuses))
      (unwrap-panic (element-at achievement-bonuses (- level u1)))
      u0))
    (fund-balance (var-get achievement-fund))
  )
    (if (and (> bonus-amount u0) (>= fund-balance bonus-amount))
      (begin
        (try! (as-contract (stx-transfer? bonus-amount tx-sender mentor)))
        (var-set achievement-fund (- fund-balance bonus-amount))
        (map-set achievement-rewards
          { mentor: mentor, achievement-type: achievement-type }
          {
            awarded-at: stacks-block-height,
            bonus-amount: bonus-amount,
            achievement-level: level
          }
        )
        (ok true)
      )
      (ok false)
    )
  )
)

(define-public (fund-achievement-system (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set achievement-fund (+ (var-get achievement-fund) amount))
    (ok true)
  )
)

(define-public (trigger-achievement-check (mentor principal))
  (begin
    (try! (check-session-milestone mentor))
    (try! (check-earnings-milestone mentor))
    (ok true)
  )
)

(define-read-only (get-mentor-achievements (mentor principal))
  (map-get? mentor-achievements { mentor: mentor })
)

(define-read-only (get-achievement-reward (mentor principal) (achievement-type (string-ascii 20)))
  (map-get? achievement-rewards { mentor: mentor, achievement-type: achievement-type })
)

(define-read-only (get-achievement-fund-balance)
  (var-get achievement-fund)
)