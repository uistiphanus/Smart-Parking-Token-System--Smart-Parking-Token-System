;; parking-analytics.clar
;; Smart Parking Analytics Dashboard for comprehensive usage insights and revenue optimization
;; Provides real-time analytics, usage patterns, and predictive recommendations

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u400))
(define-constant ERR_ANALYTICS_NOT_FOUND (err u401))
(define-constant ERR_INVALID_PERIOD (err u402))
(define-constant ERR_INSUFFICIENT_DATA (err u403))

;; Data variables for global analytics
(define-data-var total-sessions-tracked uint u0)
(define-data-var peak-hour-utilization uint u0)
(define-data-var average-session-duration uint u0)
(define-data-var peak-revenue-day uint u0)

;; Daily usage analytics per space
(define-map daily-analytics
  { space-id: uint, day: uint }
  {
    sessions-count: uint,
    total-duration: uint,
    total-revenue: uint,
    peak-hour: uint,
    utilization-rate: uint,
    average-session-length: uint
  })

;; Hourly usage patterns for predictive analytics
(define-map hourly-patterns
  { space-id: uint, hour: uint }
  {
    usage-count: uint,
    total-revenue: uint,
    average-duration: uint,
    demand-score: uint
  })

;; Space performance metrics
(define-map space-performance
  { space-id: uint }
  {
    total-sessions: uint,
    total-revenue: uint,
    average-utilization: uint,
    best-performing-hour: uint,
    revenue-trend: uint,
    customer-satisfaction: uint
  })

;; User behavior analytics
(define-map user-behavior
  { user: principal }
  {
    favorite-spaces: (list 5 uint),
    preferred-hours: (list 3 uint),
    average-session-length: uint,
    loyalty-score: uint,
    spending-pattern: uint
  })

;; Revenue forecasting data
(define-map revenue-forecast
  { space-id: uint, forecast-period: uint }
  {
    predicted-revenue: uint,
    predicted-sessions: uint,
    confidence-score: uint,
    recommended-rate: uint
  })

;; Private helper functions (defined first to avoid resolution issues)

(define-private (calculate-optimization-potential (space-id uint))
  ;; Calculate potential revenue increase with optimization
  u20) ;; Placeholder: 20% potential increase

(define-private (calculate-utilization-rate (space-id uint) (day uint))
  (let ((daily-data (map-get? daily-analytics { space-id: space-id, day: day })))
    (match daily-data
      data (/ (* (get total-duration data) u100) u1440) ;; Percentage of day utilized
      u0)))

(define-private (calculate-demand-score (space-id uint) (hour uint))
  (let ((hourly-data (map-get? hourly-patterns { space-id: space-id, hour: hour })))
    (match hourly-data
      data (+ (get usage-count data) (/ (get total-revenue data) u10))
      u1)))

(define-private (calculate-average-utilization (space-id uint))
  ;; Simplified calculation - in real implementation would use historical data
  u65) ;; Placeholder for complex calculation

(define-private (get-best-hour (space-id uint))
  ;; Find hour with highest demand score (simplified)
  u14) ;; Placeholder - would iterate through all hours

(define-private (calculate-revenue-trend (space-id uint))
  ;; Calculate trend direction: 0=declining, 1=stable, 2=growing
  u1) ;; Placeholder

(define-private (get-customer-satisfaction (space-id uint))
  ;; Get average rating for space (would integrate with rating system)
  u75) ;; Placeholder percentage

(define-private (calculate-spending-pattern (user principal))
  ;; Calculate user's spending pattern score
  u50) ;; Placeholder

(define-private (get-historical-performance (space-id uint) (days uint))
  ;; Get historical performance data
  { total-revenue: u100, total-sessions: u20 }) ;; Placeholder

(define-private (calculate-predicted-sessions (space-id uint) (forecast-days uint))
  ;; Predict future sessions based on historical data
  (* forecast-days u3)) ;; Placeholder: 3 sessions per day average

(define-private (calculate-optimal-rate (space-id uint))
  ;; Calculate optimal hourly rate based on demand
  u15) ;; Placeholder

(define-private (calculate-confidence-score (space-id uint))
  ;; Calculate confidence in forecast accuracy
  u85) ;; Placeholder: 85% confidence

(define-private (get-forecast-data (space-id uint))
  ;; Get latest forecast data
  (default-to
    { predicted-revenue: u0, predicted-sessions: u0, confidence-score: u0, recommended-rate: u10 }
    (map-get? revenue-forecast { space-id: space-id, forecast-period: u7 })))

(define-private (get-demand-pattern (space-id uint))
  ;; Get demand pattern classification: 0=low, 1=moderate, 2=high
  u1) ;; Placeholder

(define-private (update-favorite-spaces (current-spaces (list 5 uint)) (new-space uint))
  ;; Add space to favorites if not already present (simplified)
  (if (< (len current-spaces) u5)
    (unwrap-panic (as-max-len? (append current-spaces new-space) u5))
    current-spaces))

(define-private (update-preferred-hours (current-hours (list 3 uint)) (new-hour uint))
  ;; Add hour to preferred if not already present (simplified)
  (if (< (len current-hours) u3)
    (unwrap-panic (as-max-len? (append current-hours new-hour) u3))
    current-hours))

(define-private (update-user-behavior (user principal) (space-id uint) (duration uint) (hour uint))
  (let ((current-behavior (default-to
                          { favorite-spaces: (list), preferred-hours: (list),
                            average-session-length: u0, loyalty-score: u0, spending-pattern: u0 }
                          (map-get? user-behavior { user: user }))))
    (map-set user-behavior
      { user: user }
      {
        favorite-spaces: (update-favorite-spaces (get favorite-spaces current-behavior) space-id),
        preferred-hours: (update-preferred-hours (get preferred-hours current-behavior) hour),
        average-session-length: (/ (+ (* (get average-session-length current-behavior) u10) duration) u11),
        loyalty-score: (+ (get loyalty-score current-behavior) u1),
        spending-pattern: (calculate-spending-pattern user)
      })
    (ok true)))

;; Public functions

;; Record parking session analytics
(define-public (record-session-analytics 
    (space-id uint)
    (user principal)
    (duration uint)
    (revenue uint))
  (let (
    (current-day (/ stacks-block-height u144)) ;; Approximate daily blocks
    (current-hour (mod (/ stacks-block-height u6) u24)) ;; Hourly approximation
    (current-daily (default-to 
                    { sessions-count: u0, total-duration: u0, total-revenue: u0, 
                      peak-hour: u0, utilization-rate: u0, average-session-length: u0 }
                    (map-get? daily-analytics { space-id: space-id, day: current-day })))
    (current-hourly (default-to
                     { usage-count: u0, total-revenue: u0, average-duration: u0, demand-score: u0 }
                     (map-get? hourly-patterns { space-id: space-id, hour: current-hour })))
    (space-perf (default-to
                 { total-sessions: u0, total-revenue: u0, average-utilization: u0,
                   best-performing-hour: u0, revenue-trend: u0, customer-satisfaction: u0 }
                 (map-get? space-performance { space-id: space-id })))
  )
    
    ;; Update daily analytics
    (map-set daily-analytics
      { space-id: space-id, day: current-day }
      {
        sessions-count: (+ (get sessions-count current-daily) u1),
        total-duration: (+ (get total-duration current-daily) duration),
        total-revenue: (+ (get total-revenue current-daily) revenue),
        peak-hour: (if (> (get usage-count current-hourly) u3) current-hour (get peak-hour current-daily)),
        utilization-rate: (calculate-utilization-rate space-id current-day),
        average-session-length: (/ (+ (get total-duration current-daily) duration) 
                                  (+ (get sessions-count current-daily) u1))
      })
    
    ;; Update hourly patterns
    (map-set hourly-patterns
      { space-id: space-id, hour: current-hour }
      {
        usage-count: (+ (get usage-count current-hourly) u1),
        total-revenue: (+ (get total-revenue current-hourly) revenue),
        average-duration: (/ (+ (* (get average-duration current-hourly) (get usage-count current-hourly)) duration)
                            (+ (get usage-count current-hourly) u1)),
        demand-score: (calculate-demand-score space-id current-hour)
      })
    
    ;; Update space performance
    (map-set space-performance
      { space-id: space-id }
      {
        total-sessions: (+ (get total-sessions space-perf) u1),
        total-revenue: (+ (get total-revenue space-perf) revenue),
        average-utilization: (calculate-average-utilization space-id),
        best-performing-hour: (get-best-hour space-id),
        revenue-trend: (calculate-revenue-trend space-id),
        customer-satisfaction: (get-customer-satisfaction space-id)
      })
    
    ;; Update user behavior
    (unwrap-panic (update-user-behavior user space-id duration current-hour))
    
    ;; Update global counters
    (var-set total-sessions-tracked (+ (var-get total-sessions-tracked) u1))
    
    (ok true)))

;; Generate revenue forecast
(define-public (generate-revenue-forecast (space-id uint) (forecast-days uint))
  (let (
    (historical-data (get-historical-performance space-id u7)) ;; Last 7 days
    (predicted-sessions (calculate-predicted-sessions space-id forecast-days))
    (recommended-rate (calculate-optimal-rate space-id))
  )
    (asserts! (<= forecast-days u30) ERR_INVALID_PERIOD) ;; Max 30 days forecast
    
    (map-set revenue-forecast
      { space-id: space-id, forecast-period: forecast-days }
      {
        predicted-revenue: (* predicted-sessions recommended-rate),
        predicted-sessions: predicted-sessions,
        confidence-score: (calculate-confidence-score space-id),
        recommended-rate: recommended-rate
      })
    
    (ok {
      predicted-revenue: (* predicted-sessions recommended-rate),
      predicted-sessions: predicted-sessions,
      recommended-rate: recommended-rate
    })))

;; Optimize pricing based on demand patterns
(define-public (suggest-pricing-optimization (space-id uint))
  (let (
    (best-hour (get-best-hour space-id))
    (demand-pattern (get-demand-pattern space-id))
    (current-performance (unwrap! (map-get? space-performance { space-id: space-id }) ERR_ANALYTICS_NOT_FOUND))
  )
    (ok {
      peak-hour-rate: (+ (get recommended-rate (get-forecast-data space-id)) u5),
      off-peak-rate: (- (get recommended-rate (get-forecast-data space-id)) u3),
      weekend-rate: (/ (* (get recommended-rate (get-forecast-data space-id)) u120) u100), ;; 20% markup
      utilization-score: (get average-utilization current-performance),
      optimization-potential: (calculate-optimization-potential space-id)
    })))

;; Read-only functions

(define-read-only (get-daily-analytics (space-id uint) (day uint))
  (map-get? daily-analytics { space-id: space-id, day: day }))

(define-read-only (get-hourly-patterns (space-id uint) (hour uint))
  (map-get? hourly-patterns { space-id: space-id, hour: hour }))

(define-read-only (get-space-performance (space-id uint))
  (map-get? space-performance { space-id: space-id }))

(define-read-only (get-user-behavior (user principal))
  (map-get? user-behavior { user: user }))

(define-read-only (get-revenue-forecast (space-id uint) (period uint))
  (map-get? revenue-forecast { space-id: space-id, forecast-period: period }))

(define-read-only (get-platform-analytics)
  {
    total-sessions: (var-get total-sessions-tracked),
    peak-utilization: (var-get peak-hour-utilization),
    average-session-duration: (var-get average-session-duration),
    peak-revenue-day: (var-get peak-revenue-day)
  })

(define-read-only (get-space-insights (space-id uint))
  (let ((performance (map-get? space-performance { space-id: space-id })))
    (match performance
      perf (ok {
        efficiency-score: (/ (get total-revenue perf) (if (> (get total-sessions perf) u0) (get total-sessions perf) u1)),
        peak-performance-hour: (get best-performing-hour perf),
        utilization-rate: (get average-utilization perf),
        revenue-growth: (get revenue-trend perf),
        customer-rating: (get customer-satisfaction perf)
      })
      (err ERR_ANALYTICS_NOT_FOUND))))

(define-read-only (get-optimization-recommendations (space-id uint))
  (let (
    (performance (unwrap! (map-get? space-performance { space-id: space-id }) ERR_ANALYTICS_NOT_FOUND))
    (utilization (get average-utilization performance))
  )
    (ok {
      pricing-recommendation: (if (> utilization u80) "increase-rates" "maintain-rates"),
      peak-hours-strategy: "implement-dynamic-pricing",  
      marketing-focus: (if (< (get customer-satisfaction performance) u70) "improve-service" "expand-capacity"),
      revenue-potential: (calculate-optimization-potential space-id)
    })))
