;; YieldVault Pro - Automated Yield Optimization Protocol
;; A smart contract for automated yield farming and fund rebalancing across DeFi protocols

;; Define the fungible token for vault shares
(define-fungible-token vault-shares)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-funds (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-vault-not-active (err u103))
(define-constant err-insufficient-shares (err u104))

;; Minimum deposit amount (100 STX in microSTX)
(define-constant min-deposit-amount u100000000)

;; Data Variables
(define-data-var total-vault-value uint u0)
(define-data-var vault-active bool true)
(define-data-var total-vault-shares uint u0)
(define-data-var rebalance-threshold uint u10) ;; 10% threshold for rebalancing

;; Maps for tracking user deposits and protocol allocations
(define-map user-deposits principal uint)
(define-map protocol-allocations 
  { protocol: (string-ascii 20) } 
  { amount: uint, apy: uint })

;; Supported DeFi protocols with their current APY (basis points)
(define-map protocol-registry
  (string-ascii 20)
  { active: bool, min-amount: uint, current-apy: uint })

;; Events
(define-map deposit-events
  uint
  { user: principal, amount: uint, shares: uint, timestamp: uint })

;; Initialize supported protocols
(map-set protocol-registry "ALEX" { active: true, min-amount: u50000000, current-apy: u800 }) ;; 8% APY
(map-set protocol-registry "ARKADIKO" { active: true, min-amount: u30000000, current-apy: u650 }) ;; 6.5% APY
(map-set protocol-registry "STACKSWAP" { active: true, min-amount: u40000000, current-apy: u720 }) ;; 7.2% APY

;; Function 1: DEPOSIT AND MINT SHARES
;; Users deposit STX and receive vault shares proportional to their contribution
(define-public (deposit-and-mint-shares (amount uint))
  (let (
    (current-vault-value (var-get total-vault-value))
    (current-total-shares (var-get total-vault-shares))
    (shares-to-mint (if (is-eq current-total-shares u0)
                     amount ;; First deposit: 1:1 ratio
                     (/ (* amount current-total-shares) current-vault-value)))
    (user-current-deposit (default-to u0 (map-get? user-deposits tx-sender)))
  )
    ;; Validations
    (asserts! (var-get vault-active) err-vault-not-active)
    (asserts! (>= amount min-deposit-amount) err-invalid-amount)
    
    ;; Transfer STX from user to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Mint vault shares to user
    (try! (ft-mint? vault-shares shares-to-mint tx-sender))
    
    ;; Update state variables
    (var-set total-vault-value (+ current-vault-value amount))
    (var-set total-vault-shares (+ current-total-shares shares-to-mint))
    (map-set user-deposits tx-sender (+ user-current-deposit amount))
    
    ;; Emit deposit event
    (print {
      event: "deposit",
      user: tx-sender,
      amount: amount,
      shares: shares-to-mint,
      total-vault-value: (+ current-vault-value amount)
    })
    
    (ok { shares-minted: shares-to-mint, vault-value: (+ current-vault-value amount) })
  ))

;; Function 2: OPTIMIZE YIELD ALLOCATION
;; Automatically rebalances funds across protocols to maximize yield
(define-public (optimize-yield-allocation)
  (let (
    (vault-balance (var-get total-vault-value))
    (alex-protocol (unwrap-panic (map-get? protocol-registry "ALEX")))
    (arkadiko-protocol (unwrap-panic (map-get? protocol-registry "ARKADIKO")))
    (stackswap-protocol (unwrap-panic (map-get? protocol-registry "STACKSWAP")))
    
    ;; Find the protocol with highest APY
    (best-protocol (if (and (> (get current-apy alex-protocol) (get current-apy arkadiko-protocol))
                           (> (get current-apy alex-protocol) (get current-apy stackswap-protocol)))
                      "ALEX"
                      (if (> (get current-apy arkadiko-protocol) (get current-apy stackswap-protocol))
                          "ARKADIKO"
                          "STACKSWAP")))
    
    ;; Calculate optimal allocation (70% to best protocol, 15% each to others)
    (primary-allocation (/ (* vault-balance u70) u100))
    (secondary-allocation (/ (* vault-balance u15) u100))
  )
    ;; Only owner or contract can trigger optimization
    (asserts! (or (is-eq tx-sender contract-owner) (is-eq tx-sender (as-contract tx-sender))) err-owner-only)
    (asserts! (var-get vault-active) err-vault-not-active)
    (asserts! (> vault-balance u0) err-insufficient-funds)
    
    ;; Update allocations based on optimization
    (if (is-eq best-protocol "ALEX")
      (begin
        (map-set protocol-allocations { protocol: "ALEX" } 
                { amount: primary-allocation, apy: (get current-apy alex-protocol) })
        (map-set protocol-allocations { protocol: "ARKADIKO" } 
                { amount: secondary-allocation, apy: (get current-apy arkadiko-protocol) })
        (map-set protocol-allocations { protocol: "STACKSWAP" } 
                { amount: secondary-allocation, apy: (get current-apy stackswap-protocol) }))
      (if (is-eq best-protocol "ARKADIKO")
        (begin
          (map-set protocol-allocations { protocol: "ARKADIKO" } 
                  { amount: primary-allocation, apy: (get current-apy arkadiko-protocol) })
          (map-set protocol-allocations { protocol: "ALEX" } 
                  { amount: secondary-allocation, apy: (get current-apy alex-protocol) })
          (map-set protocol-allocations { protocol: "STACKSWAP" } 
                  { amount: secondary-allocation, apy: (get current-apy stackswap-protocol) }))
        (begin
          (map-set protocol-allocations { protocol: "STACKSWAP" } 
                  { amount: primary-allocation, apy: (get current-apy stackswap-protocol) })
          (map-set protocol-allocations { protocol: "ALEX" } 
                  { amount: secondary-allocation, apy: (get current-apy alex-protocol) })
          (map-set protocol-allocations { protocol: "ARKADIKO" } 
                  { amount: secondary-allocation, apy: (get current-apy arkadiko-protocol) }))))
    
    ;; Emit optimization event
    (print {
      event: "yield-optimization",
      best-protocol: best-protocol,
      primary-allocation: primary-allocation,
      secondary-allocation: secondary-allocation,
      timestamp: stacks-block-height
    })
    
    (ok {
      optimized-protocol: best-protocol,
      primary-amount: primary-allocation,
      secondary-amount: secondary-allocation,
      estimated-apy: (if (is-eq best-protocol "ALEX") 
                       (get current-apy alex-protocol)
                       (if (is-eq best-protocol "ARKADIKO")
                         (get current-apy arkadiko-protocol)
                         (get current-apy stackswap-protocol)))
    })
  ))

;; Read-only functions for querying contract state

(define-read-only (get-vault-info)
  (ok {
    total-value: (var-get total-vault-value),
    total-shares: (var-get total-vault-shares),
    active: (var-get vault-active),
    share-price: (if (> (var-get total-vault-shares) u0)
                   (/ (var-get total-vault-value) (var-get total-vault-shares))
                   u0)
  }))


(define-read-only (get-protocol-allocation (protocol (string-ascii 20)))
  (ok (map-get? protocol-allocations { protocol: protocol })))

(define-read-only (get-all-protocols)
  (ok {
    alex: (map-get? protocol-registry "ALEX"),
    arkadiko: (map-get? protocol-registry "ARKADIKO"),
    stackswap: (map-get? protocol-registry "STACKSWAP")
  }))

;; Owner-only functions
(define-public (update-protocol-apy (protocol (string-ascii 20)) (new-apy uint))
  (let ((protocol-info (unwrap! (map-get? protocol-registry protocol) err-invalid-amount)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set protocol-registry protocol (merge (unwrap! (map-get? protocol-registry protocol) err-invalid-amount) { current-apy: new-apy }))
    (ok true)))

(define-public (toggle-vault-status)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set vault-active (not (var-get vault-active)))
    (ok (var-get vault-active))))