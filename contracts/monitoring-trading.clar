;; energy-trading
;; A contract that enables peer-to-peer energy trading with transparent pricing and automated settlement
;; Part of the GridFlow decentralized energy distribution network

;; Error Codes
(define-constant ERR-NOT-REGISTERED (err u100))
(define-constant ERR-UNAUTHORIZED (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-LISTING-NOT-FOUND (err u104))
(define-constant ERR-LISTING-CLOSED (err u105))
(define-constant ERR-LISTING-ACTIVE (err u106))
(define-constant ERR-AMOUNT-TOO-SMALL (err u107))
(define-constant ERR-AMOUNT-TOO-LARGE (err u108))
(define-constant ERR-INVALID-STATE (err u109))
(define-constant ERR-DELIVERY-NOT-CONFIRMED (err u110))
(define-constant ERR-ALREADY-CONFIRMED (err u111))
(define-constant ERR-IN-DISPUTE (err u112))
(define-constant ERR-PRICE-TOO-LOW (err u113))
(define-constant ERR-CANNOT-BUY-OWN-LISTING (err u114))

;; Data Structures

;; Participant types
(define-constant PARTICIPANT-TYPE-PRODUCER u1)
(define-constant PARTICIPANT-TYPE-CONSUMER u2)
(define-constant PARTICIPANT-TYPE-BOTH u3)

;; Listing states
(define-constant LISTING-STATE-ACTIVE u1)
(define-constant LISTING-STATE-SOLD u2)
(define-constant LISTING-STATE-DELIVERED u3)
(define-constant LISTING-STATE-SETTLED u4)
(define-constant LISTING-STATE-CANCELLED u5)
(define-constant LISTING-STATE-DISPUTED u6)

;; Pricing models
(define-constant PRICING-FIXED u1)
(define-constant PRICING-AUCTION u2)

;; Stores registered participants with their type (producer, consumer, or both)
(define-map participants { address: principal } { type: uint, registered-at: uint })

;; Energy listings available on the marketplace
(define-map energy-listings
  { id: uint }
  {
    seller: principal,
    energy-amount: uint,  ;; in kWh
    price-per-unit: uint, ;; in microSTX per kWh
    pricing-model: uint,  ;; fixed or auction
    min-price: uint,      ;; for auctions
    state: uint,
    created-at: uint,
    buyer: (optional principal),
    sold-at: (optional uint),
    delivered-at: (optional uint),
    settled-at: (optional uint)
  }
)

;; Bids placed on auction listings
(define-map listing-bids
  { listing-id: uint, bidder: principal }
  {
    price-per-unit: uint,
    timestamp: uint
  }
)

;; Participant trading history
(define-map trading-history
  { address: principal }
  {
    total-energy-sold: uint,
    total-energy-bought: uint,
    successful-trades: uint,
    disputed-trades: uint,
    last-active: uint
  }
)

;; Escrow for funds during energy delivery
(define-map escrow
  { listing-id: uint }
  {
    amount: uint,
    buyer: principal,
    seller: principal,
    deposit-time: uint
  }
)

;; Contract state variables
(define-data-var listing-nonce uint u0)
(define-data-var contract-owner principal tx-sender)
(define-data-var platform-fee-percentage uint u1) ;; 1% fee by default
(define-data-var minimum-listing-amount uint u1) ;; minimum 1 kWh
(define-data-var maximum-listing-amount uint u10000) ;; maximum 10,000 kWh

;; Private Functions

;; Calculates the platform fee for a given transaction amount
(define-private (calculate-platform-fee (amount uint))
  (/ (* amount (var-get platform-fee-percentage)) u100)
)

;; Updates the trading history for a participant
(define-private (update-trading-history (address principal) (energy-amount uint) (is-seller bool) (is-disputed bool))
  (let (
    (current-history (default-to 
      { total-energy-sold: u0, total-energy-bought: u0, successful-trades: u0, disputed-trades: u0, last-active: u0 }
      (map-get? trading-history { address: address })))
    (new-history (merge current-history {
      total-energy-sold: (if is-seller 
        (+ (get total-energy-sold current-history) energy-amount)
        (get total-energy-sold current-history)),
      total-energy-bought: (if (not is-seller)
        (+ (get total-energy-bought current-history) energy-amount)
        (get total-energy-bought current-history)),
      successful-trades: (if (not is-disputed)
        (+ (get successful-trades current-history) u1)
        (get successful-trades current-history)),
      disputed-trades: (if is-disputed
        (+ (get disputed-trades current-history) u1)
        (get disputed-trades current-history)),
      last-active: block-height
    }))
  )
    (map-set trading-history { address: address } new-history)
    true
  )
)

;; Checks if a principal is registered as a participant
(define-private (is-registered (address principal))
  (is-some (map-get? participants { address: address }))
)

;; Checks if a principal is registered as a specific type
(define-private (is-registered-as (address principal) (participant-type uint))
  (let ((participant-info (map-get? participants { address: address })))
    (and 
      (is-some participant-info) 
      (or 
        (is-eq (get type (unwrap-panic participant-info)) participant-type)
        (is-eq (get type (unwrap-panic participant-info)) PARTICIPANT-TYPE-BOTH)
      )
    )
  )
)

;; Check if listing exists and is in a valid state
(define-private (is-listing-valid-for-purchase (listing-id uint))
  (match (map-get? energy-listings { id: listing-id })
    listing (is-eq (get state listing) LISTING-STATE-ACTIVE)
    false
  )
)

;; Read-Only Functions

;; Get the current listing nonce
(define-read-only (get-listing-nonce)
  (var-get listing-nonce)
)

;; Get contract owner
(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

;; Get platform fee percentage
(define-read-only (get-platform-fee-percentage)
  (var-get platform-fee-percentage)
)

;; Get participant information
(define-read-only (get-participant-info (address principal))
  (map-get? participants { address: address })
)

;; Get listing details
(define-read-only (get-listing (listing-id uint))
  (map-get? energy-listings { id: listing-id })
)

;; Get escrow information for a listing
(define-read-only (get-escrow-info (listing-id uint))
  (map-get? escrow { listing-id: listing-id })
)

;; Get participant trading history
(define-read-only (get-trading-history (address principal))
  (map-get? trading-history { address: address })
)

;; Get highest bid for an auction listing
(define-read-only (get-highest-bid (listing-id uint))
  (let ((listing (map-get? energy-listings { id: listing-id })))
    (match listing
      l 
      (if (is-eq (get pricing-model l) PRICING-AUCTION)
        ;; This is simplified - in a real implementation you'd iterate through all bids
        ;; Since Clarity doesn't support complex iterations, this would need to be expanded
        (some { price: (get price-per-unit l), bidder: (get buyer l) })
        none
      )
      none
    )
  )
)

;; Get bid information for a specific bidder
(define-read-only (get-bid (listing-id uint) (bidder principal))
  (map-get? listing-bids { listing-id: listing-id, bidder: bidder })
)

;; Public Functions

;; Register as a participant
(define-public (register-participant (participant-type uint))
  (begin
    (asserts! (or (is-eq participant-type PARTICIPANT-TYPE-PRODUCER) 
                  (is-eq participant-type PARTICIPANT-TYPE-CONSUMER) 
                  (is-eq participant-type PARTICIPANT-TYPE-BOTH)) 
      (err u200))
    (asserts! (not (is-registered tx-sender)) (err u201))
    
    (map-set participants 
      { address: tx-sender } 
      { type: participant-type, registered-at: block-height })
    
    (ok true)
  )
)

;; Update participant type
(define-public (update-participant-type (new-type uint))
  (begin
    (asserts! (is-registered tx-sender) ERR-NOT-REGISTERED)
    (asserts! (or (is-eq new-type PARTICIPANT-TYPE-PRODUCER) 
                  (is-eq new-type PARTICIPANT-TYPE-CONSUMER) 
                  (is-eq new-type PARTICIPANT-TYPE-BOTH)) 
      (err u202))
    
    (let ((current-info (unwrap-panic (map-get? participants { address: tx-sender }))))
      (map-set participants 
        { address: tx-sender } 
        (merge current-info { type: new-type }))
    )
    
    (ok true)
  )
)

;; Create a new energy listing
(define-public (create-energy-listing 
  (energy-amount uint) 
  (price-per-unit uint) 
  (pricing-model uint)
  (min-price uint))
  
  (let (
    (listing-id (+ (var-get listing-nonce) u1))
  )
    ;; Validate the inputs
    (asserts! (is-registered-as tx-sender PARTICIPANT-TYPE-PRODUCER) ERR-NOT-REGISTERED)
    (asserts! (>= energy-amount (var-get minimum-listing-amount)) ERR-AMOUNT-TOO-SMALL)
    (asserts! (<= energy-amount (var-get maximum-listing-amount)) ERR-AMOUNT-TOO-LARGE)
    (asserts! (> price-per-unit u0) ERR-PRICE-TOO-LOW)
    (asserts! (or (is-eq pricing-model PRICING-FIXED) (is-eq pricing-model PRICING-AUCTION)) (err u203))
    (asserts! (or (is-eq pricing-model PRICING-FIXED) (> min-price u0)) (err u204)) ;; min price needed for auctions
    
    ;; Create the listing
    (map-set energy-listings
      { id: listing-id }
      {
        seller: tx-sender,
        energy-amount: energy-amount,
        price-per-unit: price-per-unit,
        pricing-model: pricing-model,
        min-price: (if (is-eq pricing-model PRICING-AUCTION) min-price u0),
        state: LISTING-STATE-ACTIVE,
        created-at: block-height,
        buyer: none,
        sold-at: none,
        delivered-at: none,
        settled-at: none
      }
    )
    
    ;; Update the nonce for the next listing
    (var-set listing-nonce listing-id)
    
    (ok listing-id)
  )
)

;; Purchase energy from a fixed-price listing
(define-public (purchase-energy (listing-id uint))
  (let (
    (listing (unwrap! (map-get? energy-listings { id: listing-id }) ERR-LISTING-NOT-FOUND))
    (total-cost (* (get price-per-unit listing) (get energy-amount listing)))
    (platform-fee (calculate-platform-fee total-cost))
    (seller-amount (- total-cost platform-fee))
  )
    ;; Validate the purchase
    (asserts! (is-registered-as tx-sender PARTICIPANT-TYPE-CONSUMER) ERR-NOT-REGISTERED)
    (asserts! (is-eq (get state listing) LISTING-STATE-ACTIVE) ERR-LISTING-CLOSED)
    (asserts! (is-eq (get pricing-model listing) PRICING-FIXED) (err u205)) ;; Must use bid function for auctions
    (asserts! (not (is-eq tx-sender (get seller listing))) ERR-CANNOT-BUY-OWN-LISTING)
    
    ;; Handle payment into escrow
    (try! (stx-transfer? total-cost tx-sender (as-contract tx-sender)))
    
    ;; Update the listing state
    (map-set energy-listings
      { id: listing-id }
      (merge listing {
        state: LISTING-STATE-SOLD,
        buyer: (some tx-sender),
        sold-at: (some block-height)
      })
    )
    
    ;; Record the escrow
    (map-set escrow
      { listing-id: listing-id }
      {
        amount: total-cost,
        buyer: tx-sender,
        seller: (get seller listing),
        deposit-time: block-height
      }
    )
    
    (ok true)
  )
)



;; Finalize an auction and handle payment
(define-public (finalize-auction (listing-id uint))
  (let (
    (listing (unwrap! (map-get? energy-listings { id: listing-id }) ERR-LISTING-NOT-FOUND))
    (seller (get seller listing))
  )
    ;; Validation
    (asserts! (is-eq tx-sender seller) ERR-UNAUTHORIZED)
    (asserts! (is-eq (get state listing) LISTING-STATE-ACTIVE) ERR-LISTING-CLOSED)
    (asserts! (is-eq (get pricing-model listing) PRICING-AUCTION) (err u208))
    (asserts! (is-some (get buyer listing)) (err u209)) ;; Must have at least one bid
    
    (let (
      (winning-bidder (unwrap-panic (get buyer listing)))
      (final-price-per-unit (get price-per-unit listing))
      (total-cost (* final-price-per-unit (get energy-amount listing)))
      (platform-fee (calculate-platform-fee total-cost))
      (seller-amount (- total-cost platform-fee))
    )
      ;; Handle payment into escrow
      (try! (stx-transfer? total-cost winning-bidder (as-contract tx-sender)))
      
      ;; Update the listing state
      (map-set energy-listings
        { id: listing-id }
        (merge listing {
          state: LISTING-STATE-SOLD,
          sold-at: (some block-height)
        })
      )
      
      ;; Record the escrow
      (map-set escrow
        { listing-id: listing-id }
        {
          amount: total-cost,
          buyer: winning-bidder,
          seller: seller,
          deposit-time: block-height
        }
      )
      
      (ok true)
    )
  )
)

;; Cancel a listing (only if not sold)
(define-public (cancel-listing (listing-id uint))
  (let (
    (listing (unwrap! (map-get? energy-listings { id: listing-id }) ERR-LISTING-NOT-FOUND))
  )
    ;; Validate cancellation
    (asserts! (is-eq tx-sender (get seller listing)) ERR-UNAUTHORIZED)
    (asserts! (is-eq (get state listing) LISTING-STATE-ACTIVE) ERR-LISTING-CLOSED)
    
    ;; Update listing state
    (map-set energy-listings
      { id: listing-id }
      (merge listing {
        state: LISTING-STATE-CANCELLED
      })
    )
    
    (ok true)
  )
)

;; Confirm energy delivery by buyer
(define-public (confirm-delivery (listing-id uint))
  (let (
    (listing (unwrap! (map-get? energy-listings { id: listing-id }) ERR-LISTING-NOT-FOUND))
    (escrow-info (unwrap! (map-get? escrow { listing-id: listing-id }) (err u210)))
  )
    ;; Validate confirmation
    (asserts! (is-eq tx-sender (unwrap-panic (get buyer listing))) ERR-UNAUTHORIZED)
    (asserts! (is-eq (get state listing) LISTING-STATE-SOLD) ERR-INVALID-STATE)
    
    ;; Update listing state
    (map-set energy-listings
      { id: listing-id }
      (merge listing {
        state: LISTING-STATE-DELIVERED,
        delivered-at: (some block-height)
      })
    )
    
    (ok true)
  )
)

;; Settle payment after delivery
(define-public (settle-payment (listing-id uint))
  (let (
    (listing (unwrap! (map-get? energy-listings { id: listing-id }) ERR-LISTING-NOT-FOUND))
    (escrow-info (unwrap! (map-get? escrow { listing-id: listing-id }) (err u211)))
    (total-amount (get amount escrow-info))
    (platform-fee (calculate-platform-fee total-amount))
    (seller-amount (- total-amount platform-fee))
  )
    ;; Validate settlement
    (asserts! (or (is-eq tx-sender (get seller listing)) 
                 (is-eq tx-sender (as-contract tx-sender)) 
                 (is-eq tx-sender (var-get contract-owner))) 
              ERR-UNAUTHORIZED)
    (asserts! (is-eq (get state listing) LISTING-STATE-DELIVERED) ERR-DELIVERY-NOT-CONFIRMED)
    
    ;; Transfer funds from escrow to seller
    (try! (as-contract (stx-transfer? seller-amount (as-contract tx-sender) (get seller listing))))
    
    ;; Transfer platform fee
    (try! (as-contract (stx-transfer? platform-fee (as-contract tx-sender) (var-get contract-owner))))
    
    ;; Update trading history for both parties
    (update-trading-history (get seller listing) (get energy-amount listing) true false)
    (update-trading-history (unwrap-panic (get buyer listing)) (get energy-amount listing) false false)
    
    ;; Update listing state
    (map-set energy-listings
      { id: listing-id }
      (merge listing {
        state: LISTING-STATE-SETTLED,
        settled-at: (some block-height)
      })
    )
    
    ;; Clear escrow
    (map-delete escrow { listing-id: listing-id })
    
    (ok true)
  )
)

;; Open a dispute for a listing
(define-public (open-dispute (listing-id uint))
  (let (
    (listing (unwrap! (map-get? energy-listings { id: listing-id }) ERR-LISTING-NOT-FOUND))
  )
    ;; Validate dispute
    (asserts! (or (is-eq tx-sender (get seller listing)) 
                 (is-eq tx-sender (unwrap-panic (get buyer listing))))
              ERR-UNAUTHORIZED)
    (asserts! (or (is-eq (get state listing) LISTING-STATE-SOLD) 
                 (is-eq (get state listing) LISTING-STATE-DELIVERED))
              ERR-INVALID-STATE)
    
    ;; Update listing state
    (map-set energy-listings
      { id: listing-id }
      (merge listing {
        state: LISTING-STATE-DISPUTED
      })
    )
    
    (ok true)
  )
)

;; Admin function to resolve disputes (simplified - would be more complex in reality)
(define-public (resolve-dispute (listing-id uint) (refund-buyer bool))
  (let (
    (listing (unwrap! (map-get? energy-listings { id: listing-id }) ERR-LISTING-NOT-FOUND))
    (escrow-info (unwrap! (map-get? escrow { listing-id: listing-id }) (err u212)))
    (total-amount (get amount escrow-info))
    (buyer (unwrap-panic (get buyer listing)))
    (seller (get seller listing))
  )
    ;; Only contract owner can resolve disputes
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (asserts! (is-eq (get state listing) LISTING-STATE-DISPUTED) ERR-INVALID-STATE)
    
    ;; Transfer funds based on resolution
    (if refund-buyer
      (begin
        ;; Refund buyer
        (try! (as-contract (stx-transfer? total-amount (as-contract tx-sender) buyer)))
        
        ;; Update trading history
        (update-trading-history seller (get energy-amount listing) true true)
        (update-trading-history buyer (get energy-amount listing) false true)
      )
      (begin
        ;; Pay seller
        (let (
          (platform-fee (calculate-platform-fee total-amount))
          (seller-amount (- total-amount platform-fee))
        )
          (try! (as-contract (stx-transfer? seller-amount (as-contract tx-sender) seller)))
          (try! (as-contract (stx-transfer? platform-fee (as-contract tx-sender) (var-get contract-owner))))
          
          ;; Update trading history
          (update-trading-history seller (get energy-amount listing) true true)
          (update-trading-history buyer (get energy-amount listing) false true)
        )
      )
    )
    
    ;; Update listing state
    (map-set energy-listings
      { id: listing-id }
      (merge listing {
        state: LISTING-STATE-SETTLED,
        settled-at: (some block-height)
      })
    )
    
    ;; Clear escrow
    (map-delete escrow { listing-id: listing-id })
    
    (ok true)
  )
)

;; Admin functions

;; Update platform fee percentage
(define-public (set-platform-fee-percentage (new-fee-percentage uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (asserts! (<= new-fee-percentage u10) (err u213)) ;; Maximum 10% fee
    
    (var-set platform-fee-percentage new-fee-percentage)
    (ok true)
  )
)

;; Update minimum and maximum listing amounts
(define-public (set-listing-amount-limits (min-amount uint) (max-amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (asserts! (< min-amount max-amount) (err u214))
    
    (var-set minimum-listing-amount min-amount)
    (var-set maximum-listing-amount max-amount)
    (ok true)
  )
)

;; Transfer contract ownership
(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)