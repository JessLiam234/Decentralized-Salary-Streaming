;; title: Dec-salary-streaming
;; version: 1.0.0
;; summary: Decentralized salary streaming system for sBTC payments
;; description: Allows employers to stream sBTC payments hourly/daily to employees

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-STREAM-NOT-FOUND (err u404))
(define-constant ERR-INSUFFICIENT-BALANCE (err u402))
(define-constant ERR-STREAM-ALREADY-EXISTS (err u409))
(define-constant ERR-INVALID-PARAMETERS (err u400))
(define-constant ERR-STREAM-ENDED (err u410))
(define-constant ERR-STREAM-PAUSED (err u411))
(define-constant ERR-NOTHING-TO-CLAIM (err u412))
(define-constant ERR-ALREADY-CLAIMED (err u413))
(define-constant ERR-BONUS-NOT-FOUND (err u414))
(define-constant ERR-BONUS-ALREADY-CLAIMED (err u415))

(define-constant BLOCKS-PER-HOUR u144)
(define-constant BLOCKS-PER-DAY u3456)

(define-data-var next-stream-id uint u1)
(define-data-var total-streams uint u0)
(define-data-var total-volume uint u0)
(define-data-var next-bonus-id uint u1)

(define-map streams
  uint
  {
    employer: principal,
    employee: principal,
    rate-per-block: uint,
    start-block: uint,
    end-block: uint,
    last-claimed-block: uint,
    total-amount: uint,
    claimed-amount: uint,
    is-active: bool,
    is-paused: bool,
    stream-type: (string-ascii 10)
  }
)

(define-map employer-streams
  principal
  (list 100 uint)
)

(define-map employee-streams
  principal
  (list 100 uint)
)

(define-map stream-balances
  uint
  uint
)

(define-map user-stats
  principal
  {
    total-sent: uint,
    total-received: uint,
    active-streams: uint
  }
)

(define-map bonuses
  uint
  {
    employer: principal,
    employee: principal,
    amount: uint,
    description: (string-ascii 100),
    created-block: uint,
    expiry-block: uint,
    is-claimed: bool
  }
)

(define-map employee-bonuses
  principal
  (list 50 uint)
)

(define-private (get-user-stats (user principal))
  (default-to 
    {total-sent: u0, total-received: u0, active-streams: u0}
    (map-get? user-stats user)
  )
)

(define-private (update-user-stats (user principal) (sent uint) (received uint) (active-change int))
  (let ((current-stats (get-user-stats user)))
    (map-set user-stats user
      {
        total-sent: (+ (get total-sent current-stats) sent),
        total-received: (+ (get total-received current-stats) received),
        active-streams: (if (< active-change 0)
          (let ((decrease (to-uint (- active-change))))
            (if (>= (get active-streams current-stats) decrease)
              (- (get active-streams current-stats) decrease)
              u0))
          (+ (get active-streams current-stats) (to-uint active-change)))
      }
    )
  )
)

(define-private (add-stream-to-user-list (user principal) (stream-id uint) (list-type (string-ascii 10)))
  (let ((current-list (if (is-eq list-type "employer")
                        (default-to (list) (map-get? employer-streams user))
                        (default-to (list) (map-get? employee-streams user)))))
    (if (is-eq list-type "employer")
      (map-set employer-streams user (unwrap-panic (as-max-len? (append current-list stream-id) u100)))
      (map-set employee-streams user (unwrap-panic (as-max-len? (append current-list stream-id) u100)))
    )
  )
)

(define-private (calculate-streamable-amount (rate-per-block uint) (blocks-elapsed uint))
  (* rate-per-block blocks-elapsed)
)

(define-private (min-uint (a uint) (b uint))
  (if (< a b) a b)
)

(define-public (create-hourly-stream (employee principal) (hourly-rate uint) (duration-hours uint))
  (let 
    (
      (stream-id (var-get next-stream-id))
      (rate-per-block (/ hourly-rate BLOCKS-PER-HOUR))
      (duration-blocks (* duration-hours BLOCKS-PER-HOUR))
      (start-block stacks-block-height)
      (end-block (+ start-block duration-blocks))
      (total-amount (* hourly-rate duration-hours))
    )
    (asserts! (> hourly-rate u0) ERR-INVALID-PARAMETERS)
    (asserts! (> duration-hours u0) ERR-INVALID-PARAMETERS)
    (asserts! (not (is-eq tx-sender employee)) ERR-INVALID-PARAMETERS)
    (asserts! (is-none (map-get? streams stream-id)) ERR-STREAM-ALREADY-EXISTS)
    
    (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
    
    (map-set streams stream-id
      {
        employer: tx-sender,
        employee: employee,
        rate-per-block: rate-per-block,
        start-block: start-block,
        end-block: end-block,
        last-claimed-block: start-block,
        total-amount: total-amount,
        claimed-amount: u0,
        is-active: true,
        is-paused: false,
        stream-type: "hourly"
      }
    )
    
    (map-set stream-balances stream-id total-amount)
    (add-stream-to-user-list tx-sender stream-id "employer")
    (add-stream-to-user-list employee stream-id "employee")
    (update-user-stats tx-sender total-amount u0 1)
    (update-user-stats employee u0 u0 1)
    
    (var-set next-stream-id (+ stream-id u1))
    (var-set total-streams (+ (var-get total-streams) u1))
    (var-set total-volume (+ (var-get total-volume) total-amount))
    
    (ok stream-id)
  )
)

(define-public (create-daily-stream (employee principal) (daily-rate uint) (duration-days uint))
  (let 
    (
      (stream-id (var-get next-stream-id))
      (rate-per-block (/ daily-rate BLOCKS-PER-DAY))
      (duration-blocks (* duration-days BLOCKS-PER-DAY))
      (start-block stacks-block-height)
      (end-block (+ start-block duration-blocks))
      (total-amount (* daily-rate duration-days))
    )
    (asserts! (> daily-rate u0) ERR-INVALID-PARAMETERS)
    (asserts! (> duration-days u0) ERR-INVALID-PARAMETERS)
    (asserts! (not (is-eq tx-sender employee)) ERR-INVALID-PARAMETERS)
    (asserts! (is-none (map-get? streams stream-id)) ERR-STREAM-ALREADY-EXISTS)
    
    (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
    
    (map-set streams stream-id
      {
        employer: tx-sender,
        employee: employee,
        rate-per-block: rate-per-block,
        start-block: start-block,
        end-block: end-block,
        last-claimed-block: start-block,
        total-amount: total-amount,
        claimed-amount: u0,
        is-active: true,
        is-paused: false,
        stream-type: "daily"
      }
    )
    
    (map-set stream-balances stream-id total-amount)
    (add-stream-to-user-list tx-sender stream-id "employer")
    (add-stream-to-user-list employee stream-id "employee")
    (update-user-stats tx-sender total-amount u0 1)
    (update-user-stats employee u0 u0 1)
    
    (var-set next-stream-id (+ stream-id u1))
    (var-set total-streams (+ (var-get total-streams) u1))
    (var-set total-volume (+ (var-get total-volume) total-amount))
    
    (ok stream-id)
  )
)

(define-public (claim-payment (stream-id uint))
  (let 
    (
      (stream-data (unwrap! (map-get? streams stream-id) ERR-STREAM-NOT-FOUND))
      (current-block stacks-block-height)
      (last-claimed (get last-claimed-block stream-data))
      (end-block (get end-block stream-data))
      (rate-per-block (get rate-per-block stream-data))
      (claimed-amount (get claimed-amount stream-data))
      (total-amount (get total-amount stream-data))
      (stream-balance (default-to u0 (map-get? stream-balances stream-id)))
    )
    (asserts! (is-eq tx-sender (get employee stream-data)) ERR-NOT-AUTHORIZED)
    (asserts! (get is-active stream-data) ERR-STREAM-ENDED)
    (asserts! (not (get is-paused stream-data)) ERR-STREAM-PAUSED)
    (asserts! (< last-claimed current-block) ERR-ALREADY-CLAIMED)
    
    (let 
      (
        (claimable-until (min-uint current-block end-block))
        (blocks-elapsed (- claimable-until last-claimed))
        (claimable-amount (calculate-streamable-amount rate-per-block blocks-elapsed))
        (actual-claimable (min-uint claimable-amount stream-balance))
      )
      (asserts! (> actual-claimable u0) ERR-NOTHING-TO-CLAIM)
      (asserts! (>= stream-balance actual-claimable) ERR-INSUFFICIENT-BALANCE)
      
      (try! (as-contract (stx-transfer? actual-claimable tx-sender (get employee stream-data))))
      
      (map-set streams stream-id
        (merge stream-data
          {
            last-claimed-block: claimable-until,
            claimed-amount: (+ claimed-amount actual-claimable),
            is-active: (< claimable-until end-block)
          }
        )
      )
      
      (map-set stream-balances stream-id (- stream-balance actual-claimable))
      (update-user-stats (get employee stream-data) u0 actual-claimable 0)
      
      (if (>= claimable-until end-block)
        (update-user-stats (get employee stream-data) u0 u0 -1)
        true
      )
      
      (ok actual-claimable)
    )
  )
)

(define-public (pause-stream (stream-id uint))
  (let ((stream-data (unwrap! (map-get? streams stream-id) ERR-STREAM-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get employer stream-data)) ERR-NOT-AUTHORIZED)
    (asserts! (get is-active stream-data) ERR-STREAM-ENDED)
    (asserts! (not (get is-paused stream-data)) ERR-STREAM-PAUSED)
    
    (map-set streams stream-id (merge stream-data {is-paused: true}))
    (ok true)
  )
)

(define-public (resume-stream (stream-id uint))
  (let ((stream-data (unwrap! (map-get? streams stream-id) ERR-STREAM-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get employer stream-data)) ERR-NOT-AUTHORIZED)
    (asserts! (get is-active stream-data) ERR-STREAM-ENDED)
    (asserts! (get is-paused stream-data) ERR-STREAM-PAUSED)
    
    (map-set streams stream-id (merge stream-data {is-paused: false}))
    (ok true)
  )
)

(define-public (cancel-stream (stream-id uint))
  (let 
    (
      (stream-data (unwrap! (map-get? streams stream-id) ERR-STREAM-NOT-FOUND))
      (stream-balance (default-to u0 (map-get? stream-balances stream-id)))
    )
    (asserts! (is-eq tx-sender (get employer stream-data)) ERR-NOT-AUTHORIZED)
    (asserts! (get is-active stream-data) ERR-STREAM-ENDED)
    
    (if (> stream-balance u0)
      (try! (as-contract (stx-transfer? stream-balance tx-sender (get employer stream-data))))
      true
    )
    
    (map-set streams stream-id (merge stream-data {is-active: false}))
    (map-set stream-balances stream-id u0)
    (update-user-stats (get employer stream-data) u0 u0 -1)
    (update-user-stats (get employee stream-data) u0 u0 -1)
    
    (ok stream-balance)
  )
)

(define-read-only (get-stream (stream-id uint))
  (map-get? streams stream-id)
)

(define-read-only (get-stream-balance (stream-id uint))
  (default-to u0 (map-get? stream-balances stream-id))
)

(define-read-only (get-claimable-amount (stream-id uint))
  (match (map-get? streams stream-id)
    stream-data
    (let 
      (
        (current-block stacks-block-height)
        (last-claimed (get last-claimed-block stream-data))
        (end-block (get end-block stream-data))
        (rate-per-block (get rate-per-block stream-data))
        (stream-balance (default-to u0 (map-get? stream-balances stream-id)))
      )
      (if (and (get is-active stream-data) (not (get is-paused stream-data)))
        (let 
          (
            (claimable-until (min-uint current-block end-block))
            (blocks-elapsed (if (> claimable-until last-claimed) (- claimable-until last-claimed) u0))
            (claimable-amount (calculate-streamable-amount rate-per-block blocks-elapsed))
          )
          (min-uint claimable-amount stream-balance)
        )
        u0
      )
    )
    u0
  )
)

(define-read-only (get-user-streams (user principal) (user-type (string-ascii 10)))
  (if (is-eq user-type "employer")
    (default-to (list) (map-get? employer-streams user))
    (default-to (list) (map-get? employee-streams user))
  )
)

(define-read-only (get-user-statistics (user principal))
  (get-user-stats user)
)

(define-read-only (get-contract-stats)
  {
    total-streams: (var-get total-streams),
    total-volume: (var-get total-volume),
    next-stream-id: (var-get next-stream-id)
  }
)

(define-read-only (get-stream-progress (stream-id uint))
  (match (map-get? streams stream-id)
    stream-data
    (let 
      (
        (current-block stacks-block-height)
        (start-block (get start-block stream-data))
        (end-block (get end-block stream-data))
        (total-blocks (- end-block start-block))
        (elapsed-blocks (if (> current-block start-block) (- (min-uint current-block end-block) start-block) u0))
        (progress-percentage (if (> total-blocks u0) (/ (* elapsed-blocks u100) total-blocks) u0))
      )
      (ok {
        elapsed-blocks: elapsed-blocks,
        total-blocks: total-blocks,
        progress-percentage: progress-percentage,
        is-completed: (>= current-block end-block)
      })
    )
    ERR-STREAM-NOT-FOUND
  )
)

(define-public (issue-bonus (employee principal) (amount uint) (description (string-ascii 100)) (expiry-blocks uint))
  (let 
    (
      (bonus-id (var-get next-bonus-id))
      (current-block stacks-block-height)
      (expiry-block (+ current-block expiry-blocks))
    )
    (asserts! (> amount u0) ERR-INVALID-PARAMETERS)
    (asserts! (> expiry-blocks u0) ERR-INVALID-PARAMETERS)
    (asserts! (not (is-eq tx-sender employee)) ERR-INVALID-PARAMETERS)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set bonuses bonus-id
      {
        employer: tx-sender,
        employee: employee,
        amount: amount,
        description: description,
        created-block: current-block,
        expiry-block: expiry-block,
        is-claimed: false
      }
    )
    
    (let ((current-bonuses (default-to (list) (map-get? employee-bonuses employee))))
      (map-set employee-bonuses employee 
        (unwrap-panic (as-max-len? (append current-bonuses bonus-id) u50))
      )
    )
    
    (var-set next-bonus-id (+ bonus-id u1))
    (ok bonus-id)
  )
)

(define-public (claim-bonus (bonus-id uint))
  (let ((bonus-data (unwrap! (map-get? bonuses bonus-id) ERR-BONUS-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get employee bonus-data)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get is-claimed bonus-data)) ERR-BONUS-ALREADY-CLAIMED)
    (asserts! (< stacks-block-height (get expiry-block bonus-data)) ERR-STREAM-ENDED)
    
    (try! (as-contract (stx-transfer? (get amount bonus-data) tx-sender (get employee bonus-data))))
    
    (map-set bonuses bonus-id (merge bonus-data {is-claimed: true}))
    (update-user-stats (get employee bonus-data) u0 (get amount bonus-data) 0)
    
    (ok (get amount bonus-data))
  )
)

(define-read-only (get-bonus (bonus-id uint))
  (map-get? bonuses bonus-id)
)

(define-read-only (get-employee-bonuses (employee principal))
  (default-to (list) (map-get? employee-bonuses employee))
)

(define-read-only (get-unclaimed-bonuses (employee principal))
  (let ((bonus-list (get-employee-bonuses employee)))
    (filter is-bonus-unclaimed bonus-list)
  )
)

(define-private (is-bonus-unclaimed (bonus-id uint))
  (match (map-get? bonuses bonus-id)
    bonus-data 
    (and 
      (not (get is-claimed bonus-data))
      (< stacks-block-height (get expiry-block bonus-data))
    )
    false
  )
)
