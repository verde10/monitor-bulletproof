;; GridFlow Energy Grid Registry Contract
;; Purpose: Manage participant registration and access control for decentralized energy distribution
;; Version: 1.1.0 - Enhanced with energy trading support metadata

;;; Constants for Participant Roles
(define-constant ROLE_PRODUCER u1)
(define-constant ROLE_CONSUMER u2)
(define-constant ROLE_OPERATOR u3)
(define-constant ROLE_PROSUMER u4)  ;; New role: both producer and consumer

;;; Error Codes
(define-constant ERR_UNAUTHORIZED u401)
(define-constant ERR_ALREADY_REGISTERED u402)
(define-constant ERR_PARTICIPANT_NOT_FOUND u403)
(define-constant ERR_INVALID_REGISTRATION u404)
(define-constant ERR_INVALID_UPDATE u405)

;;; Data Structures
;; Participant metadata map with enhanced trading metadata
(define-map participants 
  principal 
  {
    role: uint,                   ;; Role of the participant
    wallet: principal,            ;; Wallet address
    energy-capacity: uint,        ;; Energy production/consumption capacity
    location: (string-utf8 100),  ;; Location details
    status: bool,                 ;; Active/Inactive status
    energy-produced: uint,        ;; Total energy produced historically
    energy-consumed: uint,        ;; Total energy consumed historically
    reputation-score: uint,       ;; Reputation score (0-100)
    last-activity: uint,          ;; Timestamp of last trading activity
    successful-trades: uint       ;; Count of successful trades
  }
)

;; Contract owner (administrator)
(define-data-var contract-owner principal tx-sender)

;;; Authorization Checks
(define-private (is-contract-owner (sender principal))
  (is-eq sender (var-get contract-owner))
)

(define-private (is-participant-registered (user principal))
  (is-some (map-get? participants user))
)

;;; Helper Functions
(define-private (validate-registration 
  (wallet principal) 
  (role uint) 
  (energy-capacity uint) 
  (location (string-utf8 100))
)
  (begin
    ;; Validate role
    (asserts! 
      (or 
        (is-eq role ROLE_PRODUCER) 
        (is-eq role ROLE_CONSUMER) 
        (is-eq role ROLE_OPERATOR)
        (is-eq role ROLE_PROSUMER)
      ) 
      (err ERR_INVALID_REGISTRATION)
    )
    
    ;; Prevent duplicate registrations
    (asserts! (not (is-participant-registered wallet)) 
      (err ERR_ALREADY_REGISTERED)
    )
    
    ;; Additional validation for energy capacity
    (asserts! (> energy-capacity u0) 
      (err ERR_INVALID_REGISTRATION)
    )
    
    (ok true)
  )
)

;;; Public Functions
;; Register a new grid participant with enhanced metadata
(define-public (register-participant 
  (wallet principal) 
  (role uint) 
  (energy-capacity uint) 
  (location (string-utf8 100))
)
  (begin
    ;; Validate registration details
    (try! (validate-registration wallet role energy-capacity location))
    
    ;; Ensure only the participant can register themselves
    (asserts! (is-eq tx-sender wallet) (err ERR_UNAUTHORIZED))
    
    ;; Add participant to registry with enhanced metadata
    (map-set participants wallet {
      role: role,
      wallet: wallet,
      energy-capacity: energy-capacity,
      location: location,
      status: true,  ;; Default to active status
      energy-produced: u0,
      energy-consumed: u0,
      reputation-score: u50,  ;; Default reputation score
      last-activity: block-height,
      successful-trades: u0
    })
    
    (ok true)
  )
)

;; Update participant status (can only be done by contract owner)
(define-public (update-participant-status 
  (wallet principal) 
  (new-status bool)
)
  (begin
    ;; Check authorization (only contract owner can update status)
    (asserts! (is-contract-owner tx-sender) (err ERR_UNAUTHORIZED))
    
    ;; Ensure participant exists
    (asserts! (is-participant-registered wallet) (err ERR_PARTICIPANT_NOT_FOUND))
    
    ;; Update participant status
    (map-set participants wallet 
      (merge 
        (unwrap! (map-get? participants wallet) (err ERR_PARTICIPANT_NOT_FOUND))
        { status: new-status }
      )
    )
    
    (ok true)
  )
)

;; Update participant energy capacity
(define-public (update-energy-capacity 
  (wallet principal) 
  (new-capacity uint)
)
  (begin
    ;; Check authorization (only self or contract owner)
    (asserts! (or (is-eq tx-sender wallet) (is-contract-owner tx-sender)) 
      (err ERR_UNAUTHORIZED))
    
    ;; Ensure participant exists
    (asserts! (is-participant-registered wallet) (err ERR_PARTICIPANT_NOT_FOUND))
    
    ;; Validate capacity
    (asserts! (> new-capacity u0) (err ERR_INVALID_UPDATE))
    
    ;; Update participant capacity
    (map-set participants wallet 
      (merge 
        (unwrap! (map-get? participants wallet) (err ERR_PARTICIPANT_NOT_FOUND))
        { energy-capacity: new-capacity }
      )
    )
    
    (ok true)
  )
)

;; Update reputation score (only contract owner can do this)
(define-public (update-reputation-score
  (wallet principal)
  (new-score uint)
)
  (begin
    ;; Check authorization
    (asserts! (is-contract-owner tx-sender) (err ERR_UNAUTHORIZED))
    
    ;; Ensure participant exists
    (asserts! (is-participant-registered wallet) (err ERR_PARTICIPANT_NOT_FOUND))
    
    ;; Validate score (0-100)
    (asserts! (<= new-score u100) (err ERR_INVALID_UPDATE))
    
    ;; Update reputation score
    (map-set participants wallet 
      (merge 
        (unwrap! (map-get? participants wallet) (err ERR_PARTICIPANT_NOT_FOUND))
        { reputation-score: new-score }
      )
    )
    
    (ok true)
  )
)

;; Record successful energy transaction
(define-public (record-energy-transaction
  (producer principal)
  (consumer principal)
  (energy-amount uint)
)
  (begin
    ;; Check authorization (only contract owner can record transactions)
    (asserts! (is-contract-owner tx-sender) (err ERR_UNAUTHORIZED))
    
    ;; Ensure both participants exist
    (asserts! (is-participant-registered producer) (err ERR_PARTICIPANT_NOT_FOUND))
    (asserts! (is-participant-registered consumer) (err ERR_PARTICIPANT_NOT_FOUND))
    
    ;; Update producer stats
    (let ((producer-data (unwrap! (map-get? participants producer) (err ERR_PARTICIPANT_NOT_FOUND))))
      (map-set participants producer
        (merge producer-data {
          energy-produced: (+ (get energy-produced producer-data) energy-amount),
          successful-trades: (+ (get successful-trades producer-data) u1),
          last-activity: block-height
        })
      )
    )
    
    ;; Update consumer stats
    (let ((consumer-data (unwrap! (map-get? participants consumer) (err ERR_PARTICIPANT_NOT_FOUND))))
      (map-set participants consumer
        (merge consumer-data {
          energy-consumed: (+ (get energy-consumed consumer-data) energy-amount),
          successful-trades: (+ (get successful-trades consumer-data) u1),
          last-activity: block-height
        })
      )
    )
    
    (ok true)
  )
)

;; Remove a participant from the registry (can only be done by contract owner)
(define-public (remove-participant (wallet principal))
  (begin
    ;; Check authorization
    (asserts! (is-contract-owner tx-sender) (err ERR_UNAUTHORIZED))
    
    ;; Ensure participant exists
    (asserts! (is-participant-registered wallet) (err ERR_PARTICIPANT_NOT_FOUND))
    
    ;; Remove participant
    (map-delete participants wallet)
    
    (ok true)
  )
)

;;; Read-Only Functions
;; Get participant details
(define-read-only (get-participant-details (wallet principal))
  (map-get? participants wallet)
)

;; Check if a participant is active
(define-read-only (is-active-participant (wallet principal))
  (match (map-get? participants wallet)
    participant (get status participant)
    false
  )
)

;; Get participant trading statistics
(define-read-only (get-participant-stats (wallet principal))
  (match (map-get? participants wallet)
    participant 
    (some {
      energy-produced: (get energy-produced participant),
      energy-consumed: (get energy-consumed participant),
      reputation-score: (get reputation-score participant),
      successful-trades: (get successful-trades participant)
    })
    none
  )
)

;; Get participant activity status (active/inactive and last activity)
(define-read-only (get-participant-activity (wallet principal))
  (match (map-get? participants wallet)
    participant 
    (some {
      status: (get status participant),
      last-activity: (get last-activity participant)
    })
    none
  )
)

;; Check if participant can produce energy
(define-read-only (can-produce-energy (wallet principal))
  (match (map-get? participants wallet)
    participant 
    (and 
      (get status participant)
      (or 
        (is-eq (get role participant) ROLE_PRODUCER)
        (is-eq (get role participant) ROLE_PROSUMER)
      )
    )
    false
  )
)

;; Check if participant can consume energy
(define-read-only (can-consume-energy (wallet principal))
  (match (map-get? participants wallet)
    participant 
    (and 
      (get status participant)
      (or 
        (is-eq (get role participant) ROLE_CONSUMER)
        (is-eq (get role participant) ROLE_PROSUMER)
      )
    )
    false
  )
)

;; Transfer contract ownership (only current owner can do this)
(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-contract-owner tx-sender) (err ERR_UNAUTHORIZED))
    (var-set contract-owner new-owner)
    (ok true)
  )
)