(define-constant err-not-found (err u401))
(define-constant err-unauthorized (err u402))
(define-constant err-invalid-params (err u403))

(define-data-var leaderboard-epoch uint u1)
(define-data-var epoch-duration uint u4320)

(define-map performance-stats
  { mentor: principal }
  {
    total-revenue: uint,
    sessions-this-epoch: uint,
    avg-response-blocks: uint,
    total-applications: uint,
    acceptance-rate: uint,
    specialization-score: uint,
    last-active-block: uint,
    performance-points: uint
  }
)

(define-map epoch-leaderboard
  { epoch: uint, rank: uint }
  {
    mentor: principal,
    points: uint,
    sessions: uint,
    revenue: uint
  }
)

(define-map mentor-rankings
  { mentor: principal, epoch: uint }
  {
    rank: uint,
    percentile: uint
  }
)

(define-public (update-performance-stats (mentor principal) (revenue uint) (response-time uint))
  (let (
    (current-epoch (var-get leaderboard-epoch))
    (existing-stats (default-to
      {
        total-revenue: u0,
        sessions-this-epoch: u0,
        avg-response-blocks: u0,
        total-applications: u0,
        acceptance-rate: u0,
        specialization-score: u0,
        last-active-block: u0,
        performance-points: u0
      }
      (map-get? performance-stats { mentor: mentor })))
  )
    (let (
      (new-sessions (+ (get sessions-this-epoch existing-stats) u1))
      (new-revenue (+ (get total-revenue existing-stats) revenue))
      (new-avg-response (/ (+ (* (get avg-response-blocks existing-stats) (get sessions-this-epoch existing-stats)) response-time) new-sessions))
      (performance-points (calculate-performance-points new-sessions new-revenue new-avg-response))
    )
      (map-set performance-stats
        { mentor: mentor }
        {
          total-revenue: new-revenue,
          sessions-this-epoch: new-sessions,
          avg-response-blocks: new-avg-response,
          total-applications: (get total-applications existing-stats),
          acceptance-rate: (get acceptance-rate existing-stats),
          specialization-score: (get specialization-score existing-stats),
          last-active-block: stacks-block-height,
          performance-points: performance-points
        }
      )
      (ok performance-points)
    )
  )
)

(define-private (calculate-performance-points (sessions uint) (revenue uint) (response-time uint))
  (let (
    (session-score (* sessions u100))
    (revenue-score (/ revenue u1000000))
    (speed-bonus (if (<= response-time u50) u500 (if (<= response-time u100) u250 u0)))
  )
    (+ session-score revenue-score speed-bonus)
  )
)

(define-public (finalize-epoch-rankings (top-mentors (list 10 principal)))
  (let ((current-epoch (var-get leaderboard-epoch)))
    (begin
      (fold process-leaderboard-entry top-mentors u1)
      (var-set leaderboard-epoch (+ current-epoch u1))
      (ok true)
    )
  )
)

(define-private (process-leaderboard-entry (mentor principal) (rank uint))
  (let (
    (stats (unwrap-panic (map-get? performance-stats { mentor: mentor })))
    (current-epoch (var-get leaderboard-epoch))
  )
    (map-set epoch-leaderboard
      { epoch: current-epoch, rank: rank }
      {
        mentor: mentor,
        points: (get performance-points stats),
        sessions: (get sessions-this-epoch stats),
        revenue: (get total-revenue stats)
      }
    )
    (map-set mentor-rankings
      { mentor: mentor, epoch: current-epoch }
      {
        rank: rank,
        percentile: (- u100 (* rank u10))
      }
    )
    (+ rank u1)
  )
)

(define-read-only (get-performance-stats (mentor principal))
  (map-get? performance-stats { mentor: mentor })
)

(define-read-only (get-epoch-leaderboard (epoch uint) (rank uint))
  (map-get? epoch-leaderboard { epoch: epoch, rank: rank })
)

(define-read-only (get-mentor-rank (mentor principal) (epoch uint))
  (map-get? mentor-rankings { mentor: mentor, epoch: epoch })
)

(define-read-only (get-current-epoch)
  (ok (var-get leaderboard-epoch))
)
