;; Census Data Privacy Bridge - Secure data sharing with external research institutions

;; Error constants
(define-constant ERR_UNAUTHORIZED (err u120))
(define-constant ERR_INVALID_REQUEST (err u121))
(define-constant ERR_DATA_NOT_AVAILABLE (err u122))
(define-constant ERR_INSTITUTION_NOT_VERIFIED (err u123))
(define-constant ERR_ACCESS_EXPIRED (err u124))
(define-constant ERR_INSUFFICIENT_PERMISSIONS (err u125))
(define-constant ERR_PRIVACY_VIOLATION (err u126))
(define-constant ERR_REQUEST_NOT_FOUND (err u127))

;; Contract owner (same as main Censix contract)
(define-data-var contract-owner principal tx-sender)

;; Data variables
(define-data-var access-request-nonce uint u0)
(define-data-var aggregation-nonce uint u0)
(define-data-var min-anonymity-threshold uint u10) ;; Minimum participants for data anonymity
(define-data-var data-retention-period uint u52560) ;; ~1 year in blocks

;; Research institution registry
(define-map verified-institutions principal {
    institution-name: (string-ascii 100),
    verification-date: uint,
    research-areas: (list 10 (string-ascii 50)),
    data-access-level: uint,
    active: bool,
    verified-by: principal
})

;; Data access requests from institutions
(define-map data-access-requests uint {
    institution: principal,
    census-id: uint,
    research-purpose: (string-ascii 200),
    requested-fields: (list 15 (string-ascii 50)),
    anonymity-level: uint,
    request-date: uint,
    approval-status: uint,
    access-expires: uint,
    approved-by: (optional principal)
})

;; Anonymized data aggregations
(define-map aggregated-datasets uint {
    source-census-id: uint,
    requesting-institution: principal,
    aggregation-method: (string-ascii 30),
    participant-count: uint,
    data-fields: (list 15 (string-ascii 50)),
    anonymity-score: uint,
    created-at: uint,
    access-count: uint,
    data-hash: (buff 32)
})

;; Privacy protection settings
(define-map privacy-controls uint {
    census-id: uint,
    allow-research-access: bool,
    min-participants-required: uint,
    restricted-fields: (list 10 (string-ascii 50)),
    auto-approval-threshold: uint,
    data-expiry: uint
})

;; Data access audit trail
(define-map access-audit-log uint {
    request-id: uint,
    institution: principal,
    access-date: uint,
    data-retrieved: (string-ascii 100),
    anonymity-verified: bool,
    access-type: (string-ascii 20)
})

;; Register a research institution
(define-public (register-research-institution 
    (institution principal)
    (institution-name (string-ascii 100))
    (research-areas (list 10 (string-ascii 50)))
    (access-level uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
        (asserts! (> (len institution-name) u0) ERR_INVALID_REQUEST)
        (asserts! (<= access-level u3) ERR_INVALID_REQUEST)
        
        (map-set verified-institutions institution {
            institution-name: institution-name,
            verification-date: stacks-block-height,
            research-areas: research-areas,
            data-access-level: access-level,
            active: true,
            verified-by: tx-sender
        })
        
        (ok true)))

;; Request access to census data for research
(define-public (request-data-access 
    (census-id uint)
    (research-purpose (string-ascii 200))
    (requested-fields (list 15 (string-ascii 50)))
    (anonymity-level uint))
    (let 
        ((request-id (var-get access-request-nonce))
         (institution-data (unwrap! (map-get? verified-institutions tx-sender) ERR_INSTITUTION_NOT_VERIFIED)))
        
        (asserts! (get active institution-data) ERR_INSTITUTION_NOT_VERIFIED)
        (asserts! (> (len research-purpose) u0) ERR_INVALID_REQUEST)
        (asserts! (> (len requested-fields) u0) ERR_INVALID_REQUEST)
        (asserts! (and (>= anonymity-level u1) (<= anonymity-level u5)) ERR_INVALID_REQUEST)
        
        (map-set data-access-requests request-id {
            institution: tx-sender,
            census-id: census-id,
            research-purpose: research-purpose,
            requested-fields: requested-fields,
            anonymity-level: anonymity-level,
            request-date: stacks-block-height,
            approval-status: u0, ;; 0=pending, 1=approved, 2=denied
            access-expires: u0,
            approved-by: none
        })
        
        (var-set access-request-nonce (+ request-id u1))
        (ok request-id)))

;; Approve or deny data access request
(define-public (process-access-request 
    (request-id uint)
    (approve bool)
    (access-duration uint))
    (let 
        ((request-data (unwrap! (map-get? data-access-requests request-id) ERR_REQUEST_NOT_FOUND))
         (census-id (get census-id request-data))
         (privacy-settings (map-get? privacy-controls census-id)))
        
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get approval-status request-data) u0) ERR_INVALID_REQUEST)
        
        (if approve
            (begin
                ;; Check privacy requirements before approval
                (try! (verify-privacy-compliance request-id))
                
                (map-set data-access-requests request-id 
                    (merge request-data {
                        approval-status: u1,
                        access-expires: (+ stacks-block-height access-duration),
                        approved-by: (some tx-sender)
                    }))
                
                ;; Create aggregated dataset if approval granted
                (try! (create-aggregated-dataset request-id))
                (ok approve)
            )
            (begin
                (map-set data-access-requests request-id 
                    (merge request-data {
                        approval-status: u2,
                        approved-by: (some tx-sender)
                    }))
                (ok approve)
            )
        )))

;; Create anonymized dataset aggregation
(define-private (create-aggregated-dataset (request-id uint))
    (let 
        ((request-data (unwrap! (map-get? data-access-requests request-id) ERR_REQUEST_NOT_FOUND))
         (aggregation-id (var-get aggregation-nonce))
         (participant-count (calculate-participant-count (get census-id request-data)))
         (anonymity-score (calculate-anonymity-score participant-count (get anonymity-level request-data)))
         (data-hash (generate-data-hash request-id)))
        
        (asserts! (>= participant-count (var-get min-anonymity-threshold)) ERR_PRIVACY_VIOLATION)
        
        (map-set aggregated-datasets aggregation-id {
            source-census-id: (get census-id request-data),
            requesting-institution: (get institution request-data),
            aggregation-method: "statistical",
            participant-count: participant-count,
            data-fields: (get requested-fields request-data),
            anonymity-score: anonymity-score,
            created-at: stacks-block-height,
            access-count: u0,
            data-hash: data-hash
        })
        
        (var-set aggregation-nonce (+ aggregation-id u1))
        (ok aggregation-id)))

;; Access approved aggregated dataset
(define-public (access-aggregated-data (request-id uint))
    (let 
        ((request-data (unwrap! (map-get? data-access-requests request-id) ERR_REQUEST_NOT_FOUND))
         (institution-data (unwrap! (map-get? verified-institutions tx-sender) ERR_INSTITUTION_NOT_VERIFIED))
         (current-block stacks-block-height))
        
        (asserts! (is-eq tx-sender (get institution request-data)) ERR_UNAUTHORIZED)
        (asserts! (get active institution-data) ERR_INSTITUTION_NOT_VERIFIED)
        (asserts! (is-eq (get approval-status request-data) u1) ERR_INSUFFICIENT_PERMISSIONS)
        (asserts! (< current-block (get access-expires request-data)) ERR_ACCESS_EXPIRED)
        
        ;; Log the access for audit trail
        (try! (log-data-access request-id))
        
        ;; Return aggregated data reference
        (ok (get-aggregated-dataset-for-request request-id))))

;; Set privacy controls for a census
(define-public (set-privacy-controls 
    (census-id uint)
    (allow-research-access bool)
    (min-participants uint)
    (restricted-fields (list 10 (string-ascii 50))))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
        (asserts! (>= min-participants (var-get min-anonymity-threshold)) ERR_PRIVACY_VIOLATION)
        
        (map-set privacy-controls census-id {
            census-id: census-id,
            allow-research-access: allow-research-access,
            min-participants-required: min-participants,
            restricted-fields: restricted-fields,
            auto-approval-threshold: u100,
            data-expiry: (+ stacks-block-height (var-get data-retention-period))
        })
        
        (ok true)))

;; Private helper functions
(define-private (verify-privacy-compliance (request-id uint))
    (let 
        ((request-data (unwrap! (map-get? data-access-requests request-id) ERR_REQUEST_NOT_FOUND))
         (census-id (get census-id request-data))
         (privacy-settings (map-get? privacy-controls census-id)))
        
        (match privacy-settings
            settings 
            (begin
                (asserts! (get allow-research-access settings) ERR_INSUFFICIENT_PERMISSIONS)
                (asserts! (>= (calculate-participant-count census-id) 
                             (get min-participants-required settings)) ERR_PRIVACY_VIOLATION)
                (ok true)
            )
            (ok true) ;; No specific privacy settings, use defaults
        )))

(define-private (calculate-participant-count (census-id uint))
    ;; Simplified calculation - in reality would query main census contract
    u25)

(define-private (calculate-anonymity-score (participant-count uint) (requested-level uint))
    (let 
        ((base-score (/ (* participant-count u10) (var-get min-anonymity-threshold)))
         (level-multiplier (+ u50 (* requested-level u10))))
        (if (> (* base-score level-multiplier) u100) u100 (* base-score level-multiplier))))

(define-private (generate-data-hash (request-id uint))
    ;; Generate a unique hash for the dataset
    (sha256 (unwrap-panic (to-consensus-buff? request-id))))

(define-private (log-data-access (request-id uint))
    (let 
        ((request-data (unwrap! (map-get? data-access-requests request-id) ERR_REQUEST_NOT_FOUND))
         (audit-id (+ request-id (* stacks-block-height u1000))))
        
        (map-set access-audit-log audit-id {
            request-id: request-id,
            institution: (get institution request-data),
            access-date: stacks-block-height,
            data-retrieved: "aggregated-statistics",
            anonymity-verified: true,
            access-type: "research"
        })
        
        (ok true)))

(define-private (get-aggregated-dataset-for-request (request-id uint))
    ;; Return dataset ID that corresponds to this request
    (+ request-id u1000))

;; Read-only functions
(define-read-only (get-institution-info (institution principal))
    (map-get? verified-institutions institution))

(define-read-only (get-access-request (request-id uint))
    (map-get? data-access-requests request-id))

(define-read-only (get-aggregated-dataset (dataset-id uint))
    (map-get? aggregated-datasets dataset-id))

(define-read-only (get-privacy-controls (census-id uint))
    (map-get? privacy-controls census-id))

(define-read-only (check-access-eligibility (institution principal) (census-id uint))
    (let 
        ((institution-data (map-get? verified-institutions institution))
         (privacy-settings (map-get? privacy-controls census-id)))
        
        (match institution-data
            inst-info (and 
                (get active inst-info)
                (>= (get data-access-level inst-info) u1)
                (match privacy-settings
                    privacy (get allow-research-access privacy)
                    true ;; Default to allowing if no specific settings
                ))
            false)))

(define-read-only (get-anonymity-threshold)
    (var-get min-anonymity-threshold))

(define-read-only (calculate-required-participants (anonymity-level uint))
    (* (var-get min-anonymity-threshold) (+ u1 anonymity-level)))

(define-read-only (get-access-statistics)
    (let 
        ((total-requests (var-get access-request-nonce))
         (total-datasets (var-get aggregation-nonce)))
        {
            total-access-requests: total-requests,
            total-aggregated-datasets: total-datasets,
            min-anonymity-threshold: (var-get min-anonymity-threshold)
        }))


