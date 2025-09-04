;; Workers Compensation Automation Smart Contract
;; Employee injury insurance system with incident reporting, medical provider networks, and return-to-work coordination

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-status (err u103))
(define-constant err-already-exists (err u104))
(define-constant err-insufficient-funds (err u105))

;; Status constants
(define-constant status-active u1)
(define-constant status-inactive u2)
(define-constant status-suspended u3)

(define-constant claim-pending u1)
(define-constant claim-approved u2)
(define-constant claim-denied u3)
(define-constant claim-closed u4)

(define-constant injury-minor u1)
(define-constant injury-moderate u2)
(define-constant injury-severe u3)
(define-constant injury-critical u4)

;; data vars
(define-data-var next-employee-id uint u1)
(define-data-var next-incident-id uint u1)
(define-data-var next-provider-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var total-claims-fund uint u0)

;; data maps

;; Employee registry
(define-map employees
  { employee-id: uint }
  {
    wallet: principal,
    name: (string-ascii 100),
    department: (string-ascii 50),
    job-title: (string-ascii 50),
    hire-date: uint,
    status: uint,
    premium-paid: uint
  }
)

;; Incident reports
(define-map incidents
  { incident-id: uint }
  {
    employee-id: uint,
    reporter: principal,
    incident-date: uint,
    injury-type: uint,
    injury-severity: uint,
    description: (string-ascii 500),
    location: (string-ascii 100),
    witnesses: (list 5 principal),
    medical-attention: bool,
    status: uint
  }
)

;; Medical provider network
(define-map providers
  { provider-id: uint }
  {
    name: (string-ascii 100),
    specialization: (string-ascii 50),
    contact: (string-ascii 100),
    status: uint,
    network-tier: uint
  }
)

;; Insurance claims
(define-map claims
  { claim-id: uint }
  {
    incident-id: uint,
    employee-id: uint,
    provider-id: uint,
    claim-amount: uint,
    claim-date: uint,
    status: uint,
    approved-amount: uint,
    return-to-work-date: (optional uint)
  }
)

;; Return-to-work coordination
(define-map return-to-work
  { employee-id: uint }
  {
    claim-id: uint,
    fitness-evaluation: bool,
    work-restrictions: (string-ascii 200),
    modified-duties: (string-ascii 200),
    target-return-date: uint,
    actual-return-date: (optional uint),
    coordinator: principal
  }
)

;; Employee lookup by wallet
(define-map employee-by-wallet
  { wallet: principal }
  { employee-id: uint }
)

;; public functions

;; Register a new employee
(define-public (register-employee (wallet principal) (name (string-ascii 100)) (department (string-ascii 50)) (job-title (string-ascii 50)))
  (let
    (
      (employee-id (var-get next-employee-id))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-none (map-get? employee-by-wallet { wallet: wallet })) err-already-exists)
    
    (map-set employees
      { employee-id: employee-id }
      {
        wallet: wallet,
        name: name,
        department: department,
        job-title: job-title,
        hire-date: stacks-block-height,
        status: status-active,
        premium-paid: u0
      }
    )
    
    (map-set employee-by-wallet { wallet: wallet } { employee-id: employee-id })
    (var-set next-employee-id (+ employee-id u1))
    
    (ok employee-id)
  )
)

;; Report an incident
(define-public (report-incident 
  (employee-id uint) 
  (injury-type uint) 
  (injury-severity uint) 
  (description (string-ascii 500)) 
  (location (string-ascii 100))
  (witnesses (list 5 principal))
  (medical-attention bool)
  )
  (let
    (
      (incident-id (var-get next-incident-id))
    )
    (asserts! (is-some (map-get? employees { employee-id: employee-id })) err-not-found)
    
    (map-set incidents
      { incident-id: incident-id }
      {
        employee-id: employee-id,
        reporter: tx-sender,
        incident-date: stacks-block-height,
        injury-type: injury-type,
        injury-severity: injury-severity,
        description: description,
        location: location,
        witnesses: witnesses,
        medical-attention: medical-attention,
        status: claim-pending
      }
    )
    
    (var-set next-incident-id (+ incident-id u1))
    (ok incident-id)
  )
)

;; Add medical provider to network
(define-public (add-provider (name (string-ascii 100)) (specialization (string-ascii 50)) (contact (string-ascii 100)) (network-tier uint))
  (let
    (
      (provider-id (var-get next-provider-id))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set providers
      { provider-id: provider-id }
      {
        name: name,
        specialization: specialization,
        contact: contact,
        status: status-active,
        network-tier: network-tier
      }
    )
    
    (var-set next-provider-id (+ provider-id u1))
    (ok provider-id)
  )
)

;; Submit insurance claim
(define-public (submit-claim (incident-id uint) (provider-id uint) (claim-amount uint))
  (let
    (
      (claim-id (var-get next-claim-id))
      (incident (unwrap! (map-get? incidents { incident-id: incident-id }) err-not-found))
      (employee-id (get employee-id incident))
    )
    (asserts! (is-some (map-get? providers { provider-id: provider-id })) err-not-found)
    (asserts! (> claim-amount u0) err-invalid-status)
    
    (map-set claims
      { claim-id: claim-id }
      {
        incident-id: incident-id,
        employee-id: employee-id,
        provider-id: provider-id,
        claim-amount: claim-amount,
        claim-date: stacks-block-height,
        status: claim-pending,
        approved-amount: u0,
        return-to-work-date: none
      }
    )
    
    (var-set next-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Approve claim
(define-public (approve-claim (claim-id uint) (approved-amount uint))
  (let
    (
      (claim (unwrap! (map-get? claims { claim-id: claim-id }) err-not-found))
      (current-fund (var-get total-claims-fund))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get status claim) claim-pending) err-invalid-status)
    (asserts! (>= current-fund approved-amount) err-insufficient-funds)
    
    (map-set claims
      { claim-id: claim-id }
      (merge claim { status: claim-approved, approved-amount: approved-amount })
    )
    
    (var-set total-claims-fund (- current-fund approved-amount))
    (ok true)
  )
)

;; Coordinate return to work
(define-public (coordinate-return-to-work 
  (employee-id uint)
  (claim-id uint)
  (work-restrictions (string-ascii 200))
  (modified-duties (string-ascii 200))
  (target-return-date uint)
  )
  (let
    (
      (claim (unwrap! (map-get? claims { claim-id: claim-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get employee-id claim) employee-id) err-invalid-status)
    (asserts! (is-eq (get status claim) claim-approved) err-invalid-status)
    
    (map-set return-to-work
      { employee-id: employee-id }
      {
        claim-id: claim-id,
        fitness-evaluation: false,
        work-restrictions: work-restrictions,
        modified-duties: modified-duties,
        target-return-date: target-return-date,
        actual-return-date: none,
        coordinator: tx-sender
      }
    )
    
    (ok true)
  )
)

;; Fund the claims pool
(define-public (fund-claims-pool (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> amount u0) err-invalid-status)
    
    (var-set total-claims-fund (+ (var-get total-claims-fund) amount))
    (ok true)
  )
)

;; Complete return to work
(define-public (complete-return-to-work (employee-id uint))
  (let
    (
      (rtw (unwrap! (map-get? return-to-work { employee-id: employee-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set return-to-work
      { employee-id: employee-id }
      (merge rtw { 
        fitness-evaluation: true,
        actual-return-date: (some stacks-block-height)
      })
    )
    
    (ok true)
  )
)

;; read only functions

;; Get employee details
(define-read-only (get-employee (employee-id uint))
  (map-get? employees { employee-id: employee-id })
)

;; Get employee ID by wallet
(define-read-only (get-employee-by-wallet (wallet principal))
  (map-get? employee-by-wallet { wallet: wallet })
)

;; Get incident details
(define-read-only (get-incident (incident-id uint))
  (map-get? incidents { incident-id: incident-id })
)

;; Get provider details
(define-read-only (get-provider (provider-id uint))
  (map-get? providers { provider-id: provider-id })
)

;; Get claim details
(define-read-only (get-claim (claim-id uint))
  (map-get? claims { claim-id: claim-id })
)

;; Get return-to-work details
(define-read-only (get-return-to-work (employee-id uint))
  (map-get? return-to-work { employee-id: employee-id })
)

;; Get total claims fund
(define-read-only (get-claims-fund)
  (var-get total-claims-fund)
)

;; Get next IDs for reference
(define-read-only (get-next-employee-id)
  (var-get next-employee-id)
)

(define-read-only (get-next-incident-id)
  (var-get next-incident-id)
)

(define-read-only (get-next-provider-id)
  (var-get next-provider-id)
)

(define-read-only (get-next-claim-id)
  (var-get next-claim-id)
)
