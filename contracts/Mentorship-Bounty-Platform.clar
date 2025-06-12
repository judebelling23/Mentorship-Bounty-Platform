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
