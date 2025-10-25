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
(define-constant ERR-TOKEN-NOT-SUPPORTED (err u416))
(define-constant ERR-INVALID-TOKEN (err u417))
(define-constant ERR-TOKEN-TRANSFER-FAILED (err u418))
(define-constant ERR-EXCHANGE-RATE-NOT-SET (err u419))
(define-constant ERR-MILESTONE-NOT-FOUND (err u420))
(define-constant ERR-MILESTONE-ALREADY-COMPLETED (err u421))
(define-constant ERR-MILESTONE-NOT-APPROVED (err u422))
(define-constant ERR-INVALID-MILESTONE (err u423))
(define-constant ERR-ALL-MILESTONES-COMPLETE (err u424))

(define-constant BLOCKS-PER-HOUR u144)
(define-constant BLOCKS-PER-DAY u3456)

(define-data-var next-stream-id uint u1)
(define-data-var total-streams uint u0)
(define-data-var total-volume uint u0)
(define-data-var next-bonus-id uint u1)
(define-data-var next-token-id uint u1)
(define-data-var next-milestone-stream-id uint u1)

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

(define-map supported-tokens
  { token-id: uint }
  {
    symbol: (string-ascii 10),
    name: (string-ascii 50),
    contract-address: (optional principal),
    decimals: uint,
    is-active: bool,
    is-stx: bool
  }
)

(define-map token-exchange-rates
  { from-token: uint, to-token: uint }
  {
    rate: uint,
    last-updated: uint
  }
)

(define-map multi-token-streams
  { stream-id: uint }
  {
    payment-token-id: uint,
    original-amount: uint,
    stx-equivalent: uint
  }
)

(define-map milestone-streams
  { milestone-stream-id: uint }
  {
    employer: principal,
    employee: principal,
    total-amount: uint,
    released-amount: uint,
    current-milestone: uint,
    total-milestones: uint,
    is-active: bool,
    created-block: uint
  }
)

(define-map milestones
  { milestone-stream-id: uint, milestone-index: uint }
  {
    description: (string-ascii 200),
    amount: uint,
    is-completed: bool,
    is-approved: bool,
    submitted-block: (optional uint),
    approved-block: (optional uint),
    evidence-hash: (optional (string-ascii 64))
  }
)

(define-map employee-milestone-streams
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

(define-public (register-token 
  (symbol (string-ascii 10))
  (name (string-ascii 50))
  (contract-address (optional principal))
  (decimals uint)
  (is-stx bool))
  (let
    (
      (token-id (var-get next-token-id))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> (len symbol) u0) ERR-INVALID-TOKEN)
    (asserts! (> (len name) u0) ERR-INVALID-TOKEN)
    (asserts! (<= decimals u18) ERR-INVALID-TOKEN)
    
    (map-set supported-tokens
      { token-id: token-id }
      {
        symbol: symbol,
        name: name,
        contract-address: contract-address,
        decimals: decimals,
        is-active: true,
        is-stx: is-stx
      }
    )
    (var-set next-token-id (+ token-id u1))
    (ok token-id)
  )
)

(define-public (toggle-token-status (token-id uint))
  (let
    (
      (token (unwrap! (map-get? supported-tokens { token-id: token-id }) ERR-TOKEN-NOT-SUPPORTED))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    (map-set supported-tokens
      { token-id: token-id }
      (merge token { is-active: (not (get is-active token)) })
    )
    (ok true)
  )
)

(define-public (set-exchange-rate 
  (from-token uint)
  (to-token uint)
  (rate uint))
  (let
    (
      (from-token-data (unwrap! (map-get? supported-tokens { token-id: from-token }) ERR-TOKEN-NOT-SUPPORTED))
      (to-token-data (unwrap! (map-get? supported-tokens { token-id: to-token }) ERR-TOKEN-NOT-SUPPORTED))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (get is-active from-token-data) ERR-TOKEN-NOT-SUPPORTED)
    (asserts! (get is-active to-token-data) ERR-TOKEN-NOT-SUPPORTED)
    (asserts! (> rate u0) ERR-INVALID-TOKEN)
    (asserts! (not (is-eq from-token to-token)) ERR-INVALID-TOKEN)
    
    (map-set token-exchange-rates
      { from-token: from-token, to-token: to-token }
      {
        rate: rate,
        last-updated: current-block
      }
    )
    (ok true)
  )
)

(define-public (create-multi-token-hourly-stream 
  (employee principal)
  (hourly-rate uint)
  (duration-hours uint)
  (payment-token-id uint))
  (let
    (
      (stream-id (var-get next-stream-id))
      (token-data (unwrap! (map-get? supported-tokens { token-id: payment-token-id }) ERR-TOKEN-NOT-SUPPORTED))
      (rate-per-block (/ hourly-rate BLOCKS-PER-HOUR))
      (duration-blocks (* duration-hours BLOCKS-PER-HOUR))
      (start-block stacks-block-height)
      (end-block (+ start-block duration-blocks))
      (total-amount (* hourly-rate duration-hours))
      (stx-equivalent (if (get is-stx token-data)
        total-amount
        (get-stx-equivalent payment-token-id total-amount)))
    )
    (asserts! (> hourly-rate u0) ERR-INVALID-PARAMETERS)
    (asserts! (> duration-hours u0) ERR-INVALID-PARAMETERS)
    (asserts! (not (is-eq tx-sender employee)) ERR-INVALID-PARAMETERS)
    (asserts! (is-none (map-get? streams stream-id)) ERR-STREAM-ALREADY-EXISTS)
    (asserts! (get is-active token-data) ERR-TOKEN-NOT-SUPPORTED)
    
    (if (get is-stx token-data)
      (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
      (try! (stx-transfer? stx-equivalent tx-sender (as-contract tx-sender)))
    )
    
    (map-set streams stream-id
      {
        employer: tx-sender,
        employee: employee,
        rate-per-block: rate-per-block,
        start-block: start-block,
        end-block: end-block,
        last-claimed-block: start-block,
        total-amount: stx-equivalent,
        claimed-amount: u0,
        is-active: true,
        is-paused: false,
        stream-type: "hourly"
      }
    )
    
    (map-set stream-balances stream-id stx-equivalent)
    (map-set multi-token-streams
      { stream-id: stream-id }
      {
        payment-token-id: payment-token-id,
        original-amount: total-amount,
        stx-equivalent: stx-equivalent
      }
    )
    
    (add-stream-to-user-list tx-sender stream-id "employer")
    (add-stream-to-user-list employee stream-id "employee")
    (update-user-stats tx-sender stx-equivalent u0 1)
    (update-user-stats employee u0 u0 1)
    
    (var-set next-stream-id (+ stream-id u1))
    (var-set total-streams (+ (var-get total-streams) u1))
    (var-set total-volume (+ (var-get total-volume) stx-equivalent))
    
    (ok stream-id)
  )
)

(define-public (create-multi-token-daily-stream 
  (employee principal)
  (daily-rate uint)
  (duration-days uint)
  (payment-token-id uint))
  (let
    (
      (stream-id (var-get next-stream-id))
      (token-data (unwrap! (map-get? supported-tokens { token-id: payment-token-id }) ERR-TOKEN-NOT-SUPPORTED))
      (rate-per-block (/ daily-rate BLOCKS-PER-DAY))
      (duration-blocks (* duration-days BLOCKS-PER-DAY))
      (start-block stacks-block-height)
      (end-block (+ start-block duration-blocks))
      (total-amount (* daily-rate duration-days))
      (stx-equivalent (if (get is-stx token-data)
        total-amount
        (get-stx-equivalent payment-token-id total-amount)))
    )
    (asserts! (> daily-rate u0) ERR-INVALID-PARAMETERS)
    (asserts! (> duration-days u0) ERR-INVALID-PARAMETERS)
    (asserts! (not (is-eq tx-sender employee)) ERR-INVALID-PARAMETERS)
    (asserts! (is-none (map-get? streams stream-id)) ERR-STREAM-ALREADY-EXISTS)
    (asserts! (get is-active token-data) ERR-TOKEN-NOT-SUPPORTED)
    
    (if (get is-stx token-data)
      (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
      (try! (stx-transfer? stx-equivalent tx-sender (as-contract tx-sender)))
    )
    
    (map-set streams stream-id
      {
        employer: tx-sender,
        employee: employee,
        rate-per-block: rate-per-block,
        start-block: start-block,
        end-block: end-block,
        last-claimed-block: start-block,
        total-amount: stx-equivalent,
        claimed-amount: u0,
        is-active: true,
        is-paused: false,
        stream-type: "daily"
      }
    )
    
    (map-set stream-balances stream-id stx-equivalent)
    (map-set multi-token-streams
      { stream-id: stream-id }
      {
        payment-token-id: payment-token-id,
        original-amount: total-amount,
        stx-equivalent: stx-equivalent
      }
    )
    
    (add-stream-to-user-list tx-sender stream-id "employer")
    (add-stream-to-user-list employee stream-id "employee")
    (update-user-stats tx-sender stx-equivalent u0 1)
    (update-user-stats employee u0 u0 1)
    
    (var-set next-stream-id (+ stream-id u1))
    (var-set total-streams (+ (var-get total-streams) u1))
    (var-set total-volume (+ (var-get total-volume) stx-equivalent))
    
    (ok stream-id)
  )
)

(define-read-only (get-supported-token (token-id uint))
  (map-get? supported-tokens { token-id: token-id })
)

(define-read-only (get-exchange-rate (from-token uint) (to-token uint))
  (map-get? token-exchange-rates { from-token: from-token, to-token: to-token })
)

(define-read-only (get-multi-token-stream-info (stream-id uint))
  (map-get? multi-token-streams { stream-id: stream-id })
)

(define-read-only (get-stx-equivalent (token-id uint) (amount uint))
  (let
    (
      (token-data (map-get? supported-tokens { token-id: token-id }))
      (exchange-rate (map-get? token-exchange-rates { from-token: token-id, to-token: u0 }))
    )
    (match token-data
      token-info
        (if (get is-stx token-info)
          amount
          (match exchange-rate
            rate-info (/ (* amount (get rate rate-info)) (pow u10 (get decimals token-info)))
            amount))
      amount
    )
  )
)

(define-read-only (convert-token-amount 
  (amount uint)
  (from-token uint)
  (to-token uint))
  (let
    (
      (from-token-data (map-get? supported-tokens { token-id: from-token }))
      (to-token-data (map-get? supported-tokens { token-id: to-token }))
      (exchange-rate (map-get? token-exchange-rates { from-token: from-token, to-token: to-token }))
    )
    (match from-token-data
      from-info
        (match to-token-data
          to-info
            (if (is-eq from-token to-token)
              (ok amount)
              (match exchange-rate
                rate-info
                  (ok (/ (* amount (get rate rate-info)) (pow u10 (get decimals from-info))))
                ERR-EXCHANGE-RATE-NOT-SET))
          ERR-TOKEN-NOT-SUPPORTED)
      ERR-TOKEN-NOT-SUPPORTED
    )
  )
)

(define-read-only (get-stream-token-info (stream-id uint))
  (let
    (
      (stream-data (map-get? streams stream-id))
      (token-info (map-get? multi-token-streams { stream-id: stream-id }))
    )
    (match stream-data
      stream-info
        (match token-info
          multi-token-data
            (let
              (
                (token-data (map-get? supported-tokens { token-id: (get payment-token-id multi-token-data) }))
              )
              (match token-data
                token-details
                  (some {
                    payment-token: token-details,
                    original-amount: (get original-amount multi-token-data),
                    stx-equivalent: (get stx-equivalent multi-token-data),
                    stream-data: stream-info
                  })
                none))
          (some {
            payment-token: { symbol: "STX", name: "Stacks", contract-address: none, decimals: u6, is-active: true, is-stx: true },
            original-amount: (get total-amount stream-info),
            stx-equivalent: (get total-amount stream-info),
            stream-data: stream-info
          }))
      none
    )
  )
)

(define-read-only (get-next-token-id)
  (var-get next-token-id)
)

(define-public (create-milestone-stream
  (employee principal)
  (total-amount uint)
  (num-milestones uint))
  (let
    (
      (milestone-stream-id (var-get next-milestone-stream-id))
    )
    (asserts! (> num-milestones u0) ERR-INVALID-PARAMETERS)
    (asserts! (<= num-milestones u10) ERR-INVALID-PARAMETERS)
    (asserts! (> total-amount u0) ERR-INVALID-PARAMETERS)
    (asserts! (not (is-eq tx-sender employee)) ERR-INVALID-PARAMETERS)
    
    (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
    
    (map-set milestone-streams
      { milestone-stream-id: milestone-stream-id }
      {
        employer: tx-sender,
        employee: employee,
        total-amount: total-amount,
        released-amount: u0,
        current-milestone: u0,
        total-milestones: num-milestones,
        is-active: true,
        created-block: stacks-block-height
      }
    )
    
    (let ((current-streams (default-to (list) (map-get? employee-milestone-streams employee))))
      (map-set employee-milestone-streams employee
        (unwrap-panic (as-max-len? (append current-streams milestone-stream-id) u50))
      )
    )
    
    (update-user-stats tx-sender total-amount u0 0)
    (var-set next-milestone-stream-id (+ milestone-stream-id u1))
    (ok milestone-stream-id)
  )
)

(define-public (define-milestone
  (milestone-stream-id uint)
  (milestone-index uint)
  (description (string-ascii 200))
  (amount uint))
  (let
    (
      (stream-data (unwrap! (map-get? milestone-streams { milestone-stream-id: milestone-stream-id }) ERR-STREAM-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get employer stream-data)) ERR-NOT-AUTHORIZED)
    (asserts! (< milestone-index (get total-milestones stream-data)) ERR-INVALID-MILESTONE)
    (asserts! (is-none (map-get? milestones { milestone-stream-id: milestone-stream-id, milestone-index: milestone-index })) ERR-MILESTONE-ALREADY-COMPLETED)
    (asserts! (> amount u0) ERR-INVALID-PARAMETERS)
    
    (map-set milestones
      { milestone-stream-id: milestone-stream-id, milestone-index: milestone-index }
      {
        description: description,
        amount: amount,
        is-completed: false,
        is-approved: false,
        submitted-block: none,
        approved-block: none,
        evidence-hash: none
      }
    )
    (ok true)
  )
)

(define-public (submit-milestone-completion
  (milestone-stream-id uint)
  (milestone-index uint)
  (evidence-hash (string-ascii 64)))
  (let
    (
      (stream-data (unwrap! (map-get? milestone-streams { milestone-stream-id: milestone-stream-id }) ERR-STREAM-NOT-FOUND))
      (milestone-data (unwrap! (map-get? milestones { milestone-stream-id: milestone-stream-id, milestone-index: milestone-index }) ERR-MILESTONE-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get employee stream-data)) ERR-NOT-AUTHORIZED)
    (asserts! (get is-active stream-data) ERR-STREAM-ENDED)
    (asserts! (is-eq milestone-index (get current-milestone stream-data)) ERR-INVALID-MILESTONE)
    (asserts! (not (get is-completed milestone-data)) ERR-MILESTONE-ALREADY-COMPLETED)
    
    (map-set milestones
      { milestone-stream-id: milestone-stream-id, milestone-index: milestone-index }
      (merge milestone-data {
        is-completed: true,
        submitted-block: (some stacks-block-height),
        evidence-hash: (some evidence-hash)
      })
    )
    (ok true)
  )
)

(define-public (approve-milestone
  (milestone-stream-id uint)
  (milestone-index uint))
  (let
    (
      (stream-data (unwrap! (map-get? milestone-streams { milestone-stream-id: milestone-stream-id }) ERR-STREAM-NOT-FOUND))
      (milestone-data (unwrap! (map-get? milestones { milestone-stream-id: milestone-stream-id, milestone-index: milestone-index }) ERR-MILESTONE-NOT-FOUND))
      (milestone-amount (get amount milestone-data))
    )
    (asserts! (is-eq tx-sender (get employer stream-data)) ERR-NOT-AUTHORIZED)
    (asserts! (get is-active stream-data) ERR-STREAM-ENDED)
    (asserts! (get is-completed milestone-data) ERR-MILESTONE-NOT-APPROVED)
    (asserts! (not (get is-approved milestone-data)) ERR-MILESTONE-ALREADY-COMPLETED)
    (asserts! (is-eq milestone-index (get current-milestone stream-data)) ERR-INVALID-MILESTONE)
    
    (try! (as-contract (stx-transfer? milestone-amount tx-sender (get employee stream-data))))
    
    (map-set milestones
      { milestone-stream-id: milestone-stream-id, milestone-index: milestone-index }
      (merge milestone-data {
        is-approved: true,
        approved-block: (some stacks-block-height)
      })
    )
    
    (let
      (
        (new-released (+ (get released-amount stream-data) milestone-amount))
        (next-milestone (+ milestone-index u1))
        (is-final-milestone (is-eq next-milestone (get total-milestones stream-data)))
      )
      (map-set milestone-streams
        { milestone-stream-id: milestone-stream-id }
        (merge stream-data {
          released-amount: new-released,
          current-milestone: next-milestone,
          is-active: (not is-final-milestone)
        })
      )
      
      (update-user-stats (get employee stream-data) u0 milestone-amount 0)
      (ok milestone-amount)
    )
  )
)

(define-public (reject-milestone
  (milestone-stream-id uint)
  (milestone-index uint))
  (let
    (
      (stream-data (unwrap! (map-get? milestone-streams { milestone-stream-id: milestone-stream-id }) ERR-STREAM-NOT-FOUND))
      (milestone-data (unwrap! (map-get? milestones { milestone-stream-id: milestone-stream-id, milestone-index: milestone-index }) ERR-MILESTONE-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get employer stream-data)) ERR-NOT-AUTHORIZED)
    (asserts! (get is-active stream-data) ERR-STREAM-ENDED)
    (asserts! (get is-completed milestone-data) ERR-MILESTONE-NOT-APPROVED)
    (asserts! (not (get is-approved milestone-data)) ERR-MILESTONE-ALREADY-COMPLETED)
    
    (map-set milestones
      { milestone-stream-id: milestone-stream-id, milestone-index: milestone-index }
      (merge milestone-data {
        is-completed: false,
        submitted-block: none,
        evidence-hash: none
      })
    )
    (ok true)
  )
)

(define-public (cancel-milestone-stream (milestone-stream-id uint))
  (let
    (
      (stream-data (unwrap! (map-get? milestone-streams { milestone-stream-id: milestone-stream-id }) ERR-STREAM-NOT-FOUND))
      (remaining-amount (- (get total-amount stream-data) (get released-amount stream-data)))
    )
    (asserts! (is-eq tx-sender (get employer stream-data)) ERR-NOT-AUTHORIZED)
    (asserts! (get is-active stream-data) ERR-STREAM-ENDED)
    
    (if (> remaining-amount u0)
      (try! (as-contract (stx-transfer? remaining-amount tx-sender (get employer stream-data))))
      true
    )
    
    (map-set milestone-streams
      { milestone-stream-id: milestone-stream-id }
      (merge stream-data { is-active: false })
    )
    (ok remaining-amount)
  )
)

(define-read-only (get-milestone-stream (milestone-stream-id uint))
  (map-get? milestone-streams { milestone-stream-id: milestone-stream-id })
)

(define-read-only (get-milestone
  (milestone-stream-id uint)
  (milestone-index uint))
  (map-get? milestones { milestone-stream-id: milestone-stream-id, milestone-index: milestone-index })
)

(define-read-only (get-employee-milestone-streams (employee principal))
  (default-to (list) (map-get? employee-milestone-streams employee))
)

(define-read-only (get-milestone-stream-progress (milestone-stream-id uint))
  (match (map-get? milestone-streams { milestone-stream-id: milestone-stream-id })
    stream-data
    (let
      (
        (completion-percentage (if (> (get total-amount stream-data) u0)
          (/ (* (get released-amount stream-data) u100) (get total-amount stream-data))
          u0))
      )
      (some {
        current-milestone: (get current-milestone stream-data),
        total-milestones: (get total-milestones stream-data),
        released-amount: (get released-amount stream-data),
        total-amount: (get total-amount stream-data),
        completion-percentage: completion-percentage,
        is-active: (get is-active stream-data)
      })
    )
    none
  )
)

(define-read-only (get-pending-milestone
  (milestone-stream-id uint))
  (match (map-get? milestone-streams { milestone-stream-id: milestone-stream-id })
    stream-data
    (let
      (
        (current-idx (get current-milestone stream-data))
      )
      (if (< current-idx (get total-milestones stream-data))
        (map-get? milestones { milestone-stream-id: milestone-stream-id, milestone-index: current-idx })
        none
      )
    )
    none
  )
)

(define-read-only (get-next-milestone-stream-id)
  (var-get next-milestone-stream-id)
)
