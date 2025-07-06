(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-registered (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-space-occupied (err u105))
(define-constant err-not-parked (err u106))
(define-constant err-invalid-duration (err u107))
(define-constant err-space-not-registered (err u108))

(define-data-var hourly-rate uint u10)
(define-data-var total-revenue uint u0)
(define-data-var total-spaces uint u0)

(define-map parking-spaces
  { space-id: uint }
  {
    owner: principal,
    is-available: bool,
    hourly-rate: uint,
    location: (string-ascii 50)
  }
)

(define-map active-parking
  { space-id: uint }
  {
    parker: principal,
    start-time: uint,
    paid-duration: uint,
    payment: uint
  }
)

(define-map user-parking-history
  { user: principal }
  { total-sessions: uint, total-spent: uint }
)

(define-read-only (get-hourly-rate)
  (var-get hourly-rate)
)

(define-read-only (get-total-revenue)
  (var-get total-revenue)
)

(define-read-only (get-total-spaces)
  (var-get total-spaces)
)

(define-read-only (get-parking-space (space-id uint))
  (map-get? parking-spaces { space-id: space-id })
)

(define-read-only (get-active-parking (space-id uint))
  (map-get? active-parking { space-id: space-id })
)

(define-read-only (get-user-history (user principal))
  (default-to 
    { total-sessions: u0, total-spent: u0 }
    (map-get? user-parking-history { user: user })
  )
)

(define-read-only (is-space-available (space-id uint))
  (match (map-get? parking-spaces { space-id: space-id })
    space (get is-available space)
    false
  )
)

(define-read-only (calculate-parking-fee (duration uint) (space-id uint))
  (match (map-get? parking-spaces { space-id: space-id })
    space (* duration (get hourly-rate space))
    u0
  )
)

(define-public (register-parking-space (space-id uint) (location (string-ascii 50)) (custom-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-none (map-get? parking-spaces { space-id: space-id })) err-already-registered)
    
    (map-set parking-spaces
      { space-id: space-id }
      {
        owner: contract-owner,
        is-available: true,
        hourly-rate: custom-rate,
        location: location
      }
    )
    
    (var-set total-spaces (+ (var-get total-spaces) u1))
    (ok space-id)
  )
)

(define-public (update-space-availability (space-id uint) (is-available bool))
  (let ((space (unwrap! (map-get? parking-spaces { space-id: space-id }) err-not-found)))
    (asserts! (is-eq tx-sender (get owner space)) err-unauthorized)
    (asserts! (is-none (map-get? active-parking { space-id: space-id })) err-space-occupied)
    
    (map-set parking-spaces
      { space-id: space-id }
      (merge space { is-available: is-available })
    )
    (ok true)
  )
)

(define-public (update-hourly-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set hourly-rate new-rate)
    (ok new-rate)
  )
)

(define-public (park-vehicle (space-id uint) (duration uint))
  (let (
    (space (unwrap! (map-get? parking-spaces { space-id: space-id }) err-space-not-registered))
    (fee (calculate-parking-fee duration space-id))
    (user-history (get-user-history tx-sender))
  )
    (asserts! (> duration u0) err-invalid-duration)
    (asserts! (get is-available space) err-space-occupied)
    (asserts! (is-none (map-get? active-parking { space-id: space-id })) err-space-occupied)
    
    (try! (stx-transfer? fee tx-sender contract-owner))
    
    (map-set active-parking
      { space-id: space-id }
      {
        parker: tx-sender,
        start-time: stacks-block-height,
        paid-duration: duration,
        payment: fee
      }
    )
    
    (map-set parking-spaces
      { space-id: space-id }
      (merge space { is-available: false })
    )
    
    (map-set user-parking-history
      { user: tx-sender }
      {
        total-sessions: (+ (get total-sessions user-history) u1),
        total-spent: (+ (get total-spent user-history) fee)
      }
    )
    
    (let ((points-earned (calculate-points-earned fee))
          (current-points (get-user-points tx-sender)))
      (map-set user-loyalty-points
        { user: tx-sender }
        {
          total-points: (+ (get total-points current-points) points-earned),
          total-earned: (+ (get total-earned current-points) points-earned),
          total-redeemed: (get total-redeemed current-points)
        }
      )
    )
    
    (var-set total-revenue (+ (var-get total-revenue) fee))
    (ok fee)
  )
)

(define-public (end-parking (space-id uint))
  (let (
    (parking (unwrap! (map-get? active-parking { space-id: space-id }) err-not-found))
    (space (unwrap! (map-get? parking-spaces { space-id: space-id }) err-not-found))
  )
    (asserts! (is-eq tx-sender (get parker parking)) err-unauthorized)
    
    (map-delete active-parking { space-id: space-id })
    
    (map-set parking-spaces
      { space-id: space-id }
      (merge space { is-available: true })
    )
    
    (ok true)
  )
)

(define-public (extend-parking (space-id uint) (additional-duration uint))
  (let (
    (parking (unwrap! (map-get? active-parking { space-id: space-id }) err-not-found))
    (fee (calculate-parking-fee additional-duration space-id))
    (user-history (get-user-history tx-sender))
  )
    (asserts! (is-eq tx-sender (get parker parking)) err-unauthorized)
    (asserts! (> additional-duration u0) err-invalid-duration)
    
    (try! (stx-transfer? fee tx-sender contract-owner))
    
    (map-set active-parking
      { space-id: space-id }
      (merge parking {
        paid-duration: (+ (get paid-duration parking) additional-duration),
        payment: (+ (get payment parking) fee)
      })
    )
    
    (map-set user-parking-history
      { user: tx-sender }
      {
        total-sessions: (get total-sessions user-history),
        total-spent: (+ (get total-spent user-history) fee)
      }
    )
    
    (let ((points-earned (calculate-points-earned fee))
          (current-points (get-user-points tx-sender)))
      (map-set user-loyalty-points
        { user: tx-sender }
        {
          total-points: (+ (get total-points current-points) points-earned),
          total-earned: (+ (get total-earned current-points) points-earned),
          total-redeemed: (get total-redeemed current-points)
        }
      )
    )
    
    (var-set total-revenue (+ (var-get total-revenue) fee))
    (ok fee)
  )
)


(define-public (withdraw-funds (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (>= amount (var-get total-revenue)) err-insufficient-funds)
    
    (try! (stx-transfer? amount contract-owner tx-sender))
    
    (var-set total-revenue (- (var-get total-revenue) amount))
    (ok true)
  )
)
(define-public (get-parking-space-info (space-id uint))
  (let ((space (unwrap! (map-get? parking-spaces { space-id: space-id }) err-space-not-registered)))
    (ok space)
  )
)

(define-constant err-invalid-rating (err u109))
(define-constant err-already-rated (err u110))

(define-map space-ratings
  { space-id: uint }
  {
    total-ratings: uint,
    total-score: uint,
    average-rating: uint
  }
)

(define-map user-ratings
  { space-id: uint, user: principal }
  { has-rated: bool }
)

(define-read-only (get-space-rating (space-id uint))
  (default-to
    { total-ratings: u0, total-score: u0, average-rating: u0 }
    (map-get? space-ratings { space-id: space-id })
  )
)

(define-public (rate-parking-space (space-id uint) (rating uint))
  (let (
    (space (unwrap! (map-get? parking-spaces { space-id: space-id }) err-not-found))
    (current-ratings (get-space-rating space-id))
    (user-rating (default-to { has-rated: false } (map-get? user-ratings { space-id: space-id, user: tx-sender })))
  )
    (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
    (asserts! (not (get has-rated user-rating)) err-already-rated)
    
    (map-set space-ratings
      { space-id: space-id }
      {
        total-ratings: (+ (get total-ratings current-ratings) u1),
        total-score: (+ (get total-score current-ratings) rating),
        average-rating: (/ (+ (get total-score current-ratings) rating) (+ (get total-ratings current-ratings) u1))
      }
    )
    
    (map-set user-ratings
      { space-id: space-id, user: tx-sender }
      { has-rated: true }
    )
    
    (ok rating)
  )
)



(define-constant morning-start u0)
(define-constant afternoon-start u8)
(define-constant evening-start u16)

(define-map time-block-rates
  { space-id: uint, time-block: uint }
  { rate: uint }
)

(define-read-only (get-time-block-rate (space-id uint) (time-block uint))
  (default-to
    { rate: (var-get hourly-rate) }
    (map-get? time-block-rates { space-id: space-id, time-block: time-block })
  )
)

(define-public (set-time-block-rate (space-id uint) (time-block uint) (new-rate uint))
  (let ((space (unwrap! (map-get? parking-spaces { space-id: space-id }) err-not-found)))
    (asserts! (is-eq tx-sender (get owner space)) err-unauthorized)
    (asserts! (<= time-block u2) err-invalid-duration)
    
    (map-set time-block-rates
      { space-id: space-id, time-block: time-block }
      { rate: new-rate }
    )
    
    (ok new-rate)
  )
)

(define-read-only (get-current-time-block)
  (let ((current-hour (mod stacks-block-height u24)))
    (if (<= current-hour afternoon-start)
      u0
      (if (<= current-hour evening-start)
        u1
        u2
      )
    )
  )
)


(define-constant err-reservation-exists (err u111))
(define-constant err-reservation-not-found (err u112))
(define-constant err-invalid-time-slot (err u113))
(define-constant err-too-late-to-cancel (err u114))
(define-constant err-reservation-expired (err u115))

(define-data-var reservation-fee-percentage uint u10)
(define-data-var cancellation-window uint u144)

(define-map reservations
  { reservation-id: uint }
  {
    space-id: uint,
    reserver: principal,
    start-block: uint,
    duration: uint,
    reservation-fee: uint,
    total-fee: uint,
    status: (string-ascii 10),
    created-at: uint
  }
)

(define-map space-reservations
  { space-id: uint, start-block: uint }
  { reservation-id: uint }
)

(define-data-var next-reservation-id uint u1)

(define-read-only (get-reservation (reservation-id uint))
  (map-get? reservations { reservation-id: reservation-id })
)

(define-read-only (get-space-reservation (space-id uint) (start-block uint))
  (map-get? space-reservations { space-id: space-id, start-block: start-block })
)

(define-read-only (calculate-reservation-fee (space-id uint) (duration uint))
  (let ((total-fee (calculate-parking-fee duration space-id)))
    (/ (* total-fee (var-get reservation-fee-percentage)) u100)
  )
)

(define-read-only (is-time-slot-available (space-id uint) (start-block uint) (duration uint))
  (let ((end-block (+ start-block duration)))
    (and
      (is-some (map-get? parking-spaces { space-id: space-id }))
      (is-none (map-get? space-reservations { space-id: space-id, start-block: start-block }))
      (> start-block stacks-block-height)
    )
  )
)

(define-public (make-reservation (space-id uint) (start-block uint) (duration uint))
  (let (
    (space (unwrap! (map-get? parking-spaces { space-id: space-id }) err-space-not-registered))
    (reservation-id (var-get next-reservation-id))
    (reservation-fee (calculate-reservation-fee space-id duration))
    (total-fee (calculate-parking-fee duration space-id))
  )
    (asserts! (> duration u0) err-invalid-duration)
    (asserts! (> start-block (+ stacks-block-height u10)) err-invalid-time-slot)
    (asserts! (is-time-slot-available space-id start-block duration) err-reservation-exists)
    
    (try! (stx-transfer? reservation-fee tx-sender contract-owner))
    
    (map-set reservations
      { reservation-id: reservation-id }
      {
        space-id: space-id,
        reserver: tx-sender,
        start-block: start-block,
        duration: duration,
        reservation-fee: reservation-fee,
        total-fee: total-fee,
        status: "active",
        created-at: stacks-block-height
      }
    )
    
    (map-set space-reservations
      { space-id: space-id, start-block: start-block }
      { reservation-id: reservation-id }
    )
    
    (var-set next-reservation-id (+ reservation-id u1))
    (var-set total-revenue (+ (var-get total-revenue) reservation-fee))
    
    (ok reservation-id)
  )
)

(define-public (cancel-reservation (reservation-id uint))
  (let (
    (reservation (unwrap! (map-get? reservations { reservation-id: reservation-id }) err-reservation-not-found))
    (refund-amount (- (get reservation-fee reservation) (/ (get reservation-fee reservation) u10)))
  )
    (asserts! (is-eq tx-sender (get reserver reservation)) err-unauthorized)
    (asserts! (is-eq (get status reservation) "active") err-reservation-not-found)
    (asserts! 
      (< stacks-block-height (- (get start-block reservation) (var-get cancellation-window))) 
      err-too-late-to-cancel
    )
    
    (try! (stx-transfer? refund-amount contract-owner tx-sender))
    
    (map-set reservations
      { reservation-id: reservation-id }
      (merge reservation { status: "cancelled" })
    )
    
    (map-delete space-reservations 
      { space-id: (get space-id reservation), start-block: (get start-block reservation) }
    )
    
    (var-set total-revenue (- (var-get total-revenue) refund-amount))
    
    (ok refund-amount)
  )
)

(define-public (claim-reserved-parking (reservation-id uint))
  (let (
    (reservation (unwrap! (map-get? reservations { reservation-id: reservation-id }) err-reservation-not-found))
    (space (unwrap! (map-get? parking-spaces { space-id: (get space-id reservation) }) err-space-not-registered))
    (remaining-fee (- (get total-fee reservation) (get reservation-fee reservation)))
    (user-history (get-user-history tx-sender))
  )
    (asserts! (is-eq tx-sender (get reserver reservation)) err-unauthorized)
    (asserts! (is-eq (get status reservation) "active") err-reservation-not-found)
    (asserts! (>= stacks-block-height (get start-block reservation)) err-invalid-time-slot)
    (asserts! 
      (<= stacks-block-height (+ (get start-block reservation) u10)) 
      err-reservation-expired
    )
    
    (try! (stx-transfer? remaining-fee tx-sender contract-owner))
    
    (map-set active-parking
      { space-id: (get space-id reservation) }
      {
        parker: tx-sender,
        start-time: stacks-block-height,
        paid-duration: (get duration reservation),
        payment: (get total-fee reservation)
      }
    )
    
    (map-set parking-spaces
      { space-id: (get space-id reservation) }
      (merge space { is-available: false })
    )
    
    (map-set reservations
      { reservation-id: reservation-id }
      (merge reservation { status: "claimed" })
    )
    
    (map-set user-parking-history
      { user: tx-sender }
      {
        total-sessions: (+ (get total-sessions user-history) u1),
        total-spent: (+ (get total-spent user-history) (get total-fee reservation))
      }
    )
    
    (let ((points-earned (calculate-points-earned (get total-fee reservation)))
          (current-points (get-user-points tx-sender)))
      (map-set user-loyalty-points
        { user: tx-sender }
        {
          total-points: (+ (get total-points current-points) points-earned),
          total-earned: (+ (get total-earned current-points) points-earned),
          total-redeemed: (get total-redeemed current-points)
        }
      )
    )
    
    (var-set total-revenue (+ (var-get total-revenue) remaining-fee))
    
    (ok (get space-id reservation))
  )
)

(define-public (set-reservation-fee-percentage (new-percentage uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-percentage u50) err-invalid-duration)
    (var-set reservation-fee-percentage new-percentage)
    (ok new-percentage)
  )
)

(define-public (set-cancellation-window (new-window uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set cancellation-window new-window)
    (ok new-window)
  )
)

(define-read-only (get-reservation-settings)
  {
    reservation-fee-percentage: (var-get reservation-fee-percentage),
    cancellation-window: (var-get cancellation-window)
  }
)

(define-constant err-insufficient-points (err u116))
(define-constant err-invalid-points (err u117))

(define-data-var points-per-stx uint u1)
(define-data-var min-redemption-points uint u100)
(define-data-var stx-per-point uint u1)

(define-map user-loyalty-points
  { user: principal }
  { 
    total-points: uint,
    total-earned: uint,
    total-redeemed: uint
  }
)

(define-read-only (get-user-points (user principal))
  (default-to
    { total-points: u0, total-earned: u0, total-redeemed: u0 }
    (map-get? user-loyalty-points { user: user })
  )
)

(define-read-only (get-loyalty-settings)
  {
    points-per-stx: (var-get points-per-stx),
    min-redemption-points: (var-get min-redemption-points),
    stx-per-point: (var-get stx-per-point)
  }
)

(define-read-only (calculate-points-earned (payment uint))
  (* payment (var-get points-per-stx))
)

(define-read-only (calculate-redemption-value (points uint))
  (/ points (var-get stx-per-point))
)

(define-public (redeem-points (points uint))
  (let (
    (user-points (get-user-points tx-sender))
    (redemption-value (calculate-redemption-value points))
  )
    (asserts! (>= points (var-get min-redemption-points)) err-invalid-points)
    (asserts! (>= (get total-points user-points) points) err-insufficient-points)
    
    (try! (stx-transfer? redemption-value contract-owner tx-sender))
    
    (map-set user-loyalty-points
      { user: tx-sender }
      {
        total-points: (- (get total-points user-points) points),
        total-earned: (get total-earned user-points),
        total-redeemed: (+ (get total-redeemed user-points) points)
      }
    )
    
    (ok redemption-value)
  )
)

(define-public (set-loyalty-settings (new-points-per-stx uint) (new-min-redemption uint) (new-stx-per-point uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> new-points-per-stx u0) err-invalid-points)
    (asserts! (> new-min-redemption u0) err-invalid-points)
    (asserts! (> new-stx-per-point u0) err-invalid-points)
    
    (var-set points-per-stx new-points-per-stx)
    (var-set min-redemption-points new-min-redemption)
    (var-set stx-per-point new-stx-per-point)
    
    (ok true)
  )
)