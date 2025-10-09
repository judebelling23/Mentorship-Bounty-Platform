(define-constant err-not-found (err u201))
(define-constant err-unauthorized (err u202))
(define-constant err-invalid-time (err u203))
(define-constant err-already-scheduled (err u204))
(define-constant err-session-active (err u205))
(define-constant err-too-late (err u206))
(define-constant err-invalid-deposit (err u207))

(define-data-var next-session-id uint u1)
(define-data-var min-commitment-deposit uint u10000000)

(define-map scheduled-sessions
  { session-id: uint }
  {
    bounty-id: uint,
    mentor: principal,
    mentee: principal,
    scheduled-start: uint,
    session-duration: uint,
    mentor-deposit: uint,
    mentee-deposit: uint,
    status: (string-ascii 20),
    created-at: uint
  }
)

(define-map session-commitments
  { session-id: uint, participant: principal }
  {
    deposit-amount: uint,
    deposited-at: uint,
    commitment-confirmed: bool,
    attendance-confirmed: bool
  }
)

(define-map session-attendance
  { session-id: uint }
  {
    mentor-checked-in: bool,
    mentee-checked-in: bool,
    session-started: bool,
    session-completed: bool,
    check-in-window-end: uint
  }
)

(define-public (schedule-session (bounty-id uint) (mentor principal) 
                                (scheduled-start uint) (duration-hours uint)
                                (mentor-deposit uint) (mentee-deposit uint))
  (let (
    (session-id (var-get next-session-id))
    (min-deposit (var-get min-commitment-deposit))
    (session-duration (* duration-hours u3600))
  )
    (asserts! (> scheduled-start (+ stacks-block-height u10)) err-invalid-time)
    (asserts! (and (>= mentor-deposit min-deposit) (>= mentee-deposit min-deposit)) err-invalid-deposit)
    (asserts! (and (> duration-hours u0) (<= duration-hours u8)) err-invalid-time)
    
    (map-set scheduled-sessions
      { session-id: session-id }
      {
        bounty-id: bounty-id,
        mentor: mentor,
        mentee: tx-sender,
        scheduled-start: scheduled-start,
        session-duration: session-duration,
        mentor-deposit: mentor-deposit,
        mentee-deposit: mentee-deposit,
        status: "pending",
        created-at: stacks-block-height
      }
    )
    (var-set next-session-id (+ session-id u1))
    (ok session-id)
  )
)

(define-public (confirm-commitment (session-id uint))
  (let (
    (session (unwrap! (map-get? scheduled-sessions { session-id: session-id }) err-not-found))
    (participant tx-sender)
    (required-deposit (if (is-eq participant (get mentor session))
      (get mentor-deposit session)
      (get mentee-deposit session)))
  )
    (asserts! (or (is-eq participant (get mentor session)) 
                  (is-eq participant (get mentee session))) err-unauthorized)
    (asserts! (< stacks-block-height (- (get scheduled-start session) u5)) err-too-late)
    
    (try! (stx-transfer? required-deposit participant (as-contract tx-sender)))
    
    (map-set session-commitments
      { session-id: session-id, participant: participant }
      {
        deposit-amount: required-deposit,
        deposited-at: stacks-block-height,
        commitment-confirmed: true,
        attendance-confirmed: false
      }
    )
    (try! (check-session-ready session-id))
    (ok true)
  )
)

(define-private (check-session-ready (session-id uint))
  (let (
    (session (unwrap! (map-get? scheduled-sessions { session-id: session-id }) err-not-found))
    (mentor-commitment (map-get? session-commitments { session-id: session-id, participant: (get mentor session) }))
    (mentee-commitment (map-get? session-commitments { session-id: session-id, participant: (get mentee session) }))
  )
    (if (and (is-some mentor-commitment) (is-some mentee-commitment))
      (begin
        (map-set scheduled-sessions
          { session-id: session-id }
          (merge session { status: "confirmed" })
        )
        (map-set session-attendance
          { session-id: session-id }
          {
            mentor-checked-in: false,
            mentee-checked-in: false,
            session-started: false,
            session-completed: false,
            check-in-window-end: (+ (get scheduled-start session) u300)
          }
        )
        (ok true)
      )
      (ok false)
    )
  )
)

(define-public (check-in-session (session-id uint))
  (let (
    (session (unwrap! (map-get? scheduled-sessions { session-id: session-id }) err-not-found))
    (attendance (unwrap! (map-get? session-attendance { session-id: session-id }) err-not-found))
    (participant tx-sender)
    (current-time stacks-block-height)
  )
    (asserts! (or (is-eq participant (get mentor session)) 
                  (is-eq participant (get mentee session))) err-unauthorized)
    (asserts! (is-eq (get status session) "confirmed") err-invalid-time)
    (asserts! (and (>= current-time (get scheduled-start session))
                   (<= current-time (get check-in-window-end attendance))) err-invalid-time)
    
    (let (
      (is-mentor (is-eq participant (get mentor session)))
      (updated-attendance (if is-mentor
        (merge attendance { mentor-checked-in: true })
        (merge attendance { mentee-checked-in: true })))
    )
      (map-set session-attendance
        { session-id: session-id }
        updated-attendance
      )
      (if (and (get mentor-checked-in updated-attendance) (get mentee-checked-in updated-attendance))
        (begin
          (map-set session-attendance
            { session-id: session-id }
            (merge updated-attendance { session-started: true })
          )
          (map-set scheduled-sessions
            { session-id: session-id }
            (merge session { status: "active" })
          )
        )
        true
      )
    )
    (ok true)
  )
)

(define-public (complete-scheduled-session (session-id uint))
  (let (
    (session (unwrap! (map-get? scheduled-sessions { session-id: session-id }) err-not-found))
    (attendance (unwrap! (map-get? session-attendance { session-id: session-id }) err-not-found))
  )
    (asserts! (is-eq tx-sender (get mentor session)) err-unauthorized)
    (asserts! (is-eq (get status session) "active") err-session-active)
    (asserts! (get session-started attendance) err-session-active)
    
    (map-set session-attendance
      { session-id: session-id }
      (merge attendance { session-completed: true })
    )
    (map-set scheduled-sessions
      { session-id: session-id }
      (merge session { status: "completed" })
    )
    (try! (release-deposits session-id))
    (ok true)
  )
)

(define-private (release-deposits (session-id uint))
  (let (
    (session (unwrap! (map-get? scheduled-sessions { session-id: session-id }) err-not-found))
    (mentor (get mentor session))
    (mentee (get mentee session))
    (mentor-commitment (unwrap! (map-get? session-commitments { session-id: session-id, participant: mentor }) err-not-found))
    (mentee-commitment (unwrap! (map-get? session-commitments { session-id: session-id, participant: mentee }) err-not-found))
  )
    (try! (as-contract (stx-transfer? (get deposit-amount mentor-commitment) tx-sender mentor)))
    (try! (as-contract (stx-transfer? (get deposit-amount mentee-commitment) tx-sender mentee)))
    (ok true)
  )
)

(define-public (forfeit-no-show-deposits (session-id uint))
  (let (
    (session (unwrap! (map-get? scheduled-sessions { session-id: session-id }) err-not-found))
    (attendance (unwrap! (map-get? session-attendance { session-id: session-id }) err-not-found))
    (session-end-time (+ (get scheduled-start session) (get session-duration session)))
  )
    (asserts! (> stacks-block-height session-end-time) err-too-late)
    (asserts! (not (get session-completed attendance)) err-session-active)
    
    (begin
      (if (and (get mentor-checked-in attendance) (not (get mentee-checked-in attendance)))
        (unwrap-panic (refund-attending-party session-id (get mentor session)))
        true
      )
      (if (and (get mentee-checked-in attendance) (not (get mentor-checked-in attendance)))  
        (unwrap-panic (refund-attending-party session-id (get mentee session)))
        true
      )
      (map-set scheduled-sessions
        { session-id: session-id }
        (merge session { status: "forfeited" })
      )
      (ok true)
    )
  )
)

(define-private (refund-attending-party (session-id uint) (attending-party principal))
  (let (
    (commitment (unwrap! (map-get? session-commitments { session-id: session-id, participant: attending-party }) err-not-found))
  )
    (as-contract (stx-transfer? (get deposit-amount commitment) tx-sender attending-party))
  )
)

(define-read-only (get-scheduled-session (session-id uint))
  (map-get? scheduled-sessions { session-id: session-id })
)

(define-read-only (get-session-commitment (session-id uint) (participant principal))
  (map-get? session-commitments { session-id: session-id, participant: participant })
)

(define-read-only (get-session-attendance (session-id uint))
  (map-get? session-attendance { session-id: session-id })
)
