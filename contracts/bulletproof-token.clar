;; energy-token
;; A SIP-010 compliant fungible token representing tradable energy units within the GridFlow network
;; Allows for minting, transferring, and burning of energy tokens by authorized participants
;; Supports fractional units to accommodate various scales of energy trading from household to industrial levels

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NOT-TOKEN-OWNER (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-INVALID-SENDER (err u104))
(define-constant ERR-INVALID-RECIPIENT (err u105))
(define-constant ERR-UNAUTHORIZED-MINTER (err u106))
(define-constant ERR-UNAUTHORIZED-BURNER (err u107))

;; Token configuration
(define-constant TOKEN-NAME "GridFlow Energy Token")
(define-constant TOKEN-SYMBOL "GWHE") ;; GridFlow Watt-Hour Equivalent
(define-constant TOKEN-DECIMALS u6) ;; 6 decimal places allows for micro-units of energy
(define-constant TOKEN-URI "https://gridflow.network/token-metadata")

;; Contract ownership
(define-data-var contract-owner principal tx-sender)

;; Role management for minters and burners
(define-map authorized-minters principal bool)
(define-map authorized-burners principal bool)

;; Token balances
(define-map token-balances principal uint)

;; Token supply tracking
(define-data-var total-supply uint u0)

;; SIP-010 Functions

;; Returns the token name
(define-read-only (get-name)
  (ok TOKEN-NAME)
)

;; Returns the token symbol
(define-read-only (get-symbol)
  (ok TOKEN-SYMBOL)
)

;; Returns the number of decimals used
(define-read-only (get-decimals)
  (ok TOKEN-DECIMALS)
)

;; Returns the token URI that points to metadata
(define-read-only (get-token-uri)
  (ok (some TOKEN-URI))
)

;; Returns the total supply of tokens
(define-read-only (get-total-supply)
  (ok (var-get total-supply))
)

;; Returns the token balance of the specified principal
(define-read-only (get-balance (account principal))
  (ok (default-to u0 (map-get? token-balances account)))
)
;; GridFlow-specific functions

;; Mint new tokens (only callable by authorized minters)
(define-public (mint (amount uint) (recipient principal))
  (begin
    ;; Check that caller is authorized to mint
    (asserts! (is-authorized-minter tx-sender) ERR-UNAUTHORIZED-MINTER)
    
    ;; Check that amount is greater than 0
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    ;; Mint tokens to recipient
    (map-set token-balances recipient 
             (+ (default-to u0 (map-get? token-balances recipient)) amount))
    
    ;; Update total supply
    (var-set total-supply (+ (var-get total-supply) amount))
    
    (ok true)
  )
)

;; Burn tokens (only callable by token owner or authorized burners)
(define-public (burn (amount uint) (burner principal))
  (begin
    ;; Check that caller is either the burner or an authorized burner
    (asserts! (or (is-eq tx-sender burner) (is-authorized-burner tx-sender)) ERR-UNAUTHORIZED-BURNER)
    
    ;; Check that amount is greater than 0
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    ;; Check that burner has sufficient balance
    (asserts! (>= (default-to u0 (map-get? token-balances burner)) amount) ERR-INSUFFICIENT-BALANCE)
    
    ;; Burn tokens from burner
    (map-set token-balances burner 
             (- (default-to u0 (map-get? token-balances burner)) amount))
    
    ;; Update total supply
    (var-set total-supply (- (var-get total-supply) amount))
    
    (ok true)
  )
)

;; Management functions

;; Add a principal to the authorized minters list
(define-public (add-authorized-minter (minter principal))
  (begin
    ;; Only contract owner can add minters
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (map-set authorized-minters minter true)
    (ok true)
  )
)

;; Remove a principal from the authorized minters list
(define-public (remove-authorized-minter (minter principal))
  (begin
    ;; Only contract owner can remove minters
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (map-delete authorized-minters minter)
    (ok true)
  )
)

;; Add a principal to the authorized burners list
(define-public (add-authorized-burner (burner principal))
  (begin
    ;; Only contract owner can add burners
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (map-set authorized-burners burner true)
    (ok true)
  )
)

;; Remove a principal from the authorized burners list
(define-public (remove-authorized-burner (burner principal))
  (begin
    ;; Only contract owner can remove burners
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (map-delete authorized-burners burner)
    (ok true)
  )
)

;; Transfer contract ownership
(define-public (transfer-ownership (new-owner principal))
  (begin
    ;; Only current contract owner can transfer ownership
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)

;; Helper functions

;; Check if a principal is an authorized minter
(define-read-only (is-authorized-minter (minter principal))
  (default-to false (map-get? authorized-minters minter))
)

;; Check if a principal is an authorized burner
(define-read-only (is-authorized-burner (burner principal))
  (default-to false (map-get? authorized-burners burner))
)

;; SIP-010 optional function stubs

;; For token allowances - not currently implemented but part of SIP-010
(define-map token-approvals { owner: principal, spender: principal } uint)

;; Approve spending of tokens by a delegate
(define-public (approve (spender principal) (amount uint))
  (begin
    (map-set token-approvals { owner: tx-sender, spender: spender } amount)
    (ok true)
  )
)

;; Get the current allowance
(define-read-only (get-allowance (owner principal) (spender principal))
  (ok (default-to u0 (map-get? token-approvals { owner: owner, spender: spender })))
)

;; Check if an operator is approved to transfer tokens
(define-read-only (is-approved-operator (operator principal) (owner principal))
  (> (default-to u0 (map-get? token-approvals { owner: owner, spender: operator })) u0)
)