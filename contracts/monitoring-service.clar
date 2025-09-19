;; energy-metering
;; 
;; This contract implements a decentralized energy metering system for the GridFlow platform.
;; It enables secure recording of energy production and consumption data from smart meters,
;; provides verification mechanisms, and supports dispute resolution to ensure the integrity
;; of energy trading on the network.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-METER (err u101))
(define-constant ERR-METER-ALREADY-REGISTERED (err u102))
(define-constant ERR-METER-NOT-REGISTERED (err u103))
(define-constant ERR-INVALID-READING (err u104))
(define-constant ERR-VERIFICATION-EXISTS (err u105))
(define-constant ERR-NO-READING-TO-VERIFY (err u106))
(define-constant ERR-DISPUTE-EXISTS (err u107))
(define-constant ERR-NO-DISPUTE (err u108))
(define-constant ERR-NOT-VERIFIER (err u109))
(define-constant ERR-NOT-ARBITRATOR (err u110))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant METER-TYPE-PRODUCER u1)
(define-constant METER-TYPE-CONSUMER u2)

;; Data structures

;; Meter registration - tracks registered smart meters and their owners
(define-map meters
  { meter-id: (buff 32) }
  {
    owner: principal,
    meter-type: uint,  ;; 1 for producer, 2 for consumer
    location: (string-utf8 100),
    active: bool,
    registration-time: uint
  }
)

;; Energy readings - records production or consumption readings from meters
(define-map energy-readings
  { 
    meter-id: (buff 32),
    timestamp: uint
  }
  {
    energy-amount: uint,  ;; in watt-hours
    signature: (buff 64), ;; cryptographic signature from the meter
    verified: bool
  }
)

;; Verification records - tracks third-party verifications of meter readings
(define-map verifications
  {
    meter-id: (buff 32),
    reading-timestamp: uint
  }
  {
    verifier: principal,
    verification-time: uint,
    result: bool,
    notes: (string-utf8 200)
  }
)

;; Dispute records - tracks disputes and resolutions
(define-map disputes
  {
    meter-id: (buff 32),
    reading-timestamp: uint
  }
  {
    disputant: principal,
    dispute-time: uint,
    reason: (string-utf8 200),
    resolved: bool,
    resolution: (optional (string-utf8 200)),
    arbitrator: (optional principal)
  }
)

;; Track authorized verifiers who can validate meter readings
(define-map authorized-verifiers
  { verifier: principal }
  { authorized-at: uint }
)

;; Track arbitrators who can resolve disputes
(define-map authorized-arbitrators
  { arbitrator: principal }
  { authorized-at: uint }
)

;; Total energy statistics
(define-data-var total-energy-produced uint u0)
(define-data-var total-energy-consumed uint u0)

;; Private functions

;; Check if the caller is the contract owner
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT-OWNER)
)

;; Check if a meter belongs to the caller
(define-private (is-meter-owner (meter-id (buff 32)))
  (match (map-get? meters { meter-id: meter-id })
    meter (is-eq (get owner meter) tx-sender)
    false
  )
)

;; Check if caller is a verified verifier
(define-private (is-verifier)
  (is-some (map-get? authorized-verifiers { verifier: tx-sender }))
)

;; Check if caller is an authorized arbitrator
(define-private (is-arbitrator)
  (is-some (map-get? authorized-arbitrators { arbitrator: tx-sender }))
)

;; Update energy statistics based on meter type
(define-private (update-energy-stats (meter-id (buff 32)) (energy-amount uint))
  (match (map-get? meters { meter-id: meter-id })
    meter 
      (if (is-eq (get meter-type meter) METER-TYPE-PRODUCER)
        (var-set total-energy-produced (+ (var-get total-energy-produced) energy-amount))
        (var-set total-energy-consumed (+ (var-get total-energy-consumed) energy-amount))
      )
    false
  )
)

;; Read-only functions

;; Get meter details
(define-read-only (get-meter-details (meter-id (buff 32)))
  (map-get? meters { meter-id: meter-id })
)

;; Get a specific energy reading
(define-read-only (get-energy-reading (meter-id (buff 32)) (timestamp uint))
  (map-get? energy-readings { meter-id: meter-id, timestamp: timestamp })
)

;; Get verification details for a reading
(define-read-only (get-verification (meter-id (buff 32)) (reading-timestamp uint))
  (map-get? verifications { meter-id: meter-id, reading-timestamp: reading-timestamp })
)

;; Get dispute details
(define-read-only (get-dispute (meter-id (buff 32)) (reading-timestamp uint))
  (map-get? disputes { meter-id: meter-id, reading-timestamp: reading-timestamp })
)

;; Check if a principal is an authorized verifier
(define-read-only (is-authorized-verifier (verifier principal))
  (is-some (map-get? authorized-verifiers { verifier: verifier }))
)

;; Check if a principal is an authorized arbitrator
(define-read-only (is-authorized-arbitrator (arbitrator principal))
  (is-some (map-get? authorized-arbitrators { arbitrator: arbitrator }))
)

;; Get total energy statistics
(define-read-only (get-energy-statistics)
  {
    total-produced: (var-get total-energy-produced),
    total-consumed: (var-get total-energy-consumed)
  }
)

;; Public functions

;; Register a new smart meter
(define-public (register-meter (meter-id (buff 32)) (meter-type uint) (location (string-utf8 100)))
  (let
    (
      (current-time (get-block-info? time (- block-height u1)))
    )
    ;; Verify meter type is valid
    (asserts! (or (is-eq meter-type METER-TYPE-PRODUCER) (is-eq meter-type METER-TYPE-CONSUMER)) ERR-INVALID-METER)
    ;; Check meter doesn't already exist
    (asserts! (is-none (map-get? meters { meter-id: meter-id })) ERR-METER-ALREADY-REGISTERED)
    
    ;; Register the meter
    (map-set meters
      { meter-id: meter-id }
      {
        owner: tx-sender,
        meter-type: meter-type,
        location: location,
        active: true,
        registration-time: (default-to u0 current-time)
      }
    )
    (ok true)
  )
)

;; Update meter status (activate/deactivate)
(define-public (update-meter-status (meter-id (buff 32)) (active bool))
  (begin
    ;; Verify caller owns the meter
    (asserts! (is-meter-owner meter-id) ERR-NOT-AUTHORIZED)
    ;; Verify meter exists
    (asserts! (is-some (map-get? meters { meter-id: meter-id })) ERR-METER-NOT-REGISTERED)
    
    (match (map-get? meters { meter-id: meter-id })
      meter (map-set meters
        { meter-id: meter-id }
        (merge meter { active: active })
      )
      false
    )
    (ok true)
  )
)

;; Submit a new energy reading from a meter
(define-public (submit-energy-reading (meter-id (buff 32)) (timestamp uint) (energy-amount uint) (signature (buff 64)))
  (begin
    ;; Verify caller owns the meter
    (asserts! (is-meter-owner meter-id) ERR-NOT-AUTHORIZED)
    ;; Verify meter exists and is active
    (match (map-get? meters { meter-id: meter-id })
      meter (asserts! (get active meter) ERR-INVALID-METER)
      (asserts! false ERR-METER-NOT-REGISTERED)
    )
    ;; Verify energy amount is positive
    (asserts! (> energy-amount u0) ERR-INVALID-READING)
    ;; Record the energy reading
    (map-set energy-readings
      { meter-id: meter-id, timestamp: timestamp }
      {
        energy-amount: energy-amount,
        signature: signature,
        verified: false
      }
    )
    ;; Update the energy statistics
    (update-energy-stats meter-id energy-amount)
    (ok true)
  )
)

;; Add a verifier who can validate meter readings
(define-public (add-verifier (verifier principal))
  (begin
    ;; Only contract owner can add verifiers
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (map-set authorized-verifiers
      { verifier: verifier }
      { authorized-at: (default-to u0 (get-block-info? time (- block-height u1))) }
    )
    (ok true)
  )
)

;; Remove a verifier
(define-public (remove-verifier (verifier principal))
  (begin
    ;; Only contract owner can remove verifiers
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (map-delete authorized-verifiers { verifier: verifier })
    (ok true)
  )
)

;; Add an arbitrator who can resolve disputes
(define-public (add-arbitrator (arbitrator principal))
  (begin
    ;; Only contract owner can add arbitrators
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (map-set authorized-arbitrators
      { arbitrator: arbitrator }
      { authorized-at: (default-to u0 (get-block-info? time (- block-height u1))) }
    )
    (ok true)
  )
)

;; Remove an arbitrator
(define-public (remove-arbitrator (arbitrator principal))
  (begin
    ;; Only contract owner can remove arbitrators
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (map-delete authorized-arbitrators { arbitrator: arbitrator })
    (ok true)
  )
)

;; Verify a meter reading (by authorized verifiers)
(define-public (verify-reading (meter-id (buff 32)) (reading-timestamp uint) (verification-result bool) (notes (string-utf8 200)))
  (begin
    ;; Check that caller is an authorized verifier
    (asserts! (is-verifier) ERR-NOT-VERIFIER)
    ;; Verify the reading exists
    (asserts! (is-some (map-get? energy-readings { meter-id: meter-id, timestamp: reading-timestamp })) ERR-NO-READING-TO-VERIFY)
    ;; Verify no verification exists already
    (asserts! (is-none (map-get? verifications { meter-id: meter-id, reading-timestamp: reading-timestamp })) ERR-VERIFICATION-EXISTS)
    
    ;; Record the verification
    (map-set verifications
      { meter-id: meter-id, reading-timestamp: reading-timestamp }
      {
        verifier: tx-sender,
        verification-time: (default-to u0 (get-block-info? time (- block-height u1))),
        result: verification-result,
        notes: notes
      }
    )
    
    ;; Update the reading verification status
    (match (map-get? energy-readings { meter-id: meter-id, timestamp: reading-timestamp })
      reading (map-set energy-readings
        { meter-id: meter-id, timestamp: reading-timestamp }
        (merge reading { verified: verification-result })
      )
      false
    )
    
    (ok true)
  )
)

;; File a dispute for a meter reading
(define-public (file-dispute (meter-id (buff 32)) (reading-timestamp uint) (reason (string-utf8 200)))
  (begin
    ;; Verify the reading exists
    (asserts! (is-some (map-get? energy-readings { meter-id: meter-id, timestamp: reading-timestamp })) ERR-NO-READING-TO-VERIFY)
    ;; Verify no dispute exists already
    (asserts! (is-none (map-get? disputes { meter-id: meter-id, reading-timestamp: reading-timestamp })) ERR-DISPUTE-EXISTS)
    
    ;; Record the dispute
    (map-set disputes
      { meter-id: meter-id, reading-timestamp: reading-timestamp }
      {
        disputant: tx-sender,
        dispute-time: (default-to u0 (get-block-info? time (- block-height u1))),
        reason: reason,
        resolved: false,
        resolution: none,
        arbitrator: none
      }
    )
    
    (ok true)
  )
)

;; Resolve a dispute (by authorized arbitrators)
(define-public (resolve-dispute (meter-id (buff 32)) (reading-timestamp uint) (resolution (string-utf8 200)))
  (begin
    ;; Check that caller is an authorized arbitrator
    (asserts! (is-arbitrator) ERR-NOT-ARBITRATOR)
    ;; Verify the dispute exists
    (match (map-get? disputes { meter-id: meter-id, reading-timestamp: reading-timestamp })
      dispute (asserts! (not (get resolved dispute)) ERR-NO-DISPUTE)
      (asserts! false ERR-NO-DISPUTE)
    )
    
    ;; Update the dispute with resolution
    (match (map-get? disputes { meter-id: meter-id, reading-timestamp: reading-timestamp })
      dispute (map-set disputes
        { meter-id: meter-id, reading-timestamp: reading-timestamp }
        (merge dispute {
          resolved: true,
          resolution: (some resolution),
          arbitrator: (some tx-sender)
        })
      )
      false
    )
    
    (ok true)
  )
)