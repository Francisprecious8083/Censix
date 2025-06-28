(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_CENSUS (err u103))
(define-constant ERR_CENSUS_CLOSED (err u104))
(define-constant ERR_ALREADY_RESPONDED (err u105))
(define-constant ERR_NOT_REGISTERED (err u106))
(define-constant ERR_INSUFFICIENT_BALANCE (err u107))

(define-fungible-token censix-token u1000000000)

(define-data-var next-census-id uint u1)
(define-data-var registration-fee uint u1000000)
(define-data-var participation-reward uint u500000)
(define-data-var total-participants uint u0)

(define-map participants principal {
    registered-at: uint,
    total-responses: uint,
    rewards-earned: uint,
    active: bool
})

(define-map census-rounds uint {
    creator: principal,
    title: (string-ascii 256),
    description: (string-ascii 512),
    start-block: uint,
    end-block: uint,
    min-participants: uint,
    total-responses: uint,
    reward-pool: uint,
    active: bool
})

(define-map census-questions uint {
    census-id: uint,
    question: (string-ascii 256),
    options: (list 10 (string-ascii 128))
})

(define-map census-responses {census-id: uint, participant: principal} {
    response-hash: (buff 32),
    submitted-at: uint,
    verified: bool
})

(define-map participant-census-responses {participant: principal, census-id: uint} bool)

(define-map census-results uint {
    total-responses: uint,
    result-hash: (buff 32),
    finalized: bool,
    finalized-at: uint
})

(define-public (register-participant)
    (let ((current-block stacks-block-height)
          (fee (var-get registration-fee)))
        (asserts! (is-none (map-get? participants tx-sender)) ERR_ALREADY_EXISTS)
        (try! (stx-transfer? fee tx-sender CONTRACT_OWNER))
        (map-set participants tx-sender {
            registered-at: current-block,
            total-responses: u0,
            rewards-earned: u0,
            active: true
        })
        (var-set total-participants (+ (var-get total-participants) u1))
        (ok true)
    )
)

(define-public (create-census 
    (title (string-ascii 256))
    (description (string-ascii 512))
    (duration-blocks uint)
    (min-participants uint)
    (reward-pool uint))
    (let ((census-id (var-get next-census-id))
          (current-block stacks-block-height)
          (end-block (+ current-block duration-blocks)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> duration-blocks u0) ERR_INVALID_CENSUS)
        (asserts! (> min-participants u0) ERR_INVALID_CENSUS)
        (map-set census-rounds census-id {
            creator: tx-sender,
            title: title,
            description: description,
            start-block: current-block,
            end-block: end-block,
            min-participants: min-participants,
            total-responses: u0,
            reward-pool: reward-pool,
            active: true
        })
        (var-set next-census-id (+ census-id u1))
        (ok census-id)
    )
)

(define-public (add-census-question 
    (census-id uint)
    (question (string-ascii 256))
    (options (list 10 (string-ascii 128))))
    (let ((census (unwrap! (map-get? census-rounds census-id) ERR_NOT_FOUND)))
        (asserts! (is-eq tx-sender (get creator census)) ERR_UNAUTHORIZED)
        (asserts! (< stacks-block-height (get start-block census)) ERR_CENSUS_CLOSED)
        (map-set census-questions census-id {
            census-id: census-id,
            question: question,
            options: options
        })
        (ok true)
    )
)

(define-public (submit-census-response 
    (census-id uint)
    (response-hash (buff 32)))
    (let ((census (unwrap! (map-get? census-rounds census-id) ERR_NOT_FOUND))
          (participant (unwrap! (map-get? participants tx-sender) ERR_NOT_REGISTERED))
          (current-block stacks-block-height))
        (asserts! (get active participant) ERR_NOT_REGISTERED)
        (asserts! (get active census) ERR_CENSUS_CLOSED)
        (asserts! (>= current-block (get start-block census)) ERR_INVALID_CENSUS)
        (asserts! (< current-block (get end-block census)) ERR_CENSUS_CLOSED)
        (asserts! (is-none (map-get? participant-census-responses {participant: tx-sender, census-id: census-id})) ERR_ALREADY_RESPONDED)
        
        (map-set census-responses {census-id: census-id, participant: tx-sender} {
            response-hash: response-hash,
            submitted-at: current-block,
            verified: false
        })
        
        (map-set participant-census-responses {participant: tx-sender, census-id: census-id} true)
        
        (map-set census-rounds census-id 
            (merge census {total-responses: (+ (get total-responses census) u1)}))
        
        (map-set participants tx-sender 
            (merge participant {total-responses: (+ (get total-responses participant) u1)}))
        
        (ok true)
    )
)

(define-public (verify-response 
    (census-id uint)
    (participant principal))
    (let ((response (unwrap! (map-get? census-responses {census-id: census-id, participant: participant}) ERR_NOT_FOUND)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set census-responses {census-id: census-id, participant: participant}
            (merge response {verified: true}))
        (ok true)
    )
)

(define-public (finalize-census (census-id uint) (result-hash (buff 32)))
    (let ((census (unwrap! (map-get? census-rounds census-id) ERR_NOT_FOUND))
          (current-block stacks-block-height))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (>= current-block (get end-block census)) ERR_INVALID_CENSUS)
        (asserts! (>= (get total-responses census) (get min-participants census)) ERR_INVALID_CENSUS)
        
        (map-set census-results census-id {
            total-responses: (get total-responses census),
            result-hash: result-hash,
            finalized: true,
            finalized-at: current-block
        })
        
        (map-set census-rounds census-id 
            (merge census {active: false}))
        
        (ok true)
    )
)

(define-public (distribute-rewards (census-id uint) (participants-list (list 1000 principal)))
    (let ((census (unwrap! (map-get? census-rounds census-id) ERR_NOT_FOUND))
          (results (unwrap! (map-get? census-results census-id) ERR_NOT_FOUND))
          (reward-per-participant (/ (get reward-pool census) (get total-responses census))))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (get finalized results) ERR_INVALID_CENSUS)
        
        (fold distribute-single-reward participants-list {census-id: census-id, reward: reward-per-participant})
        (ok true)
    )
)

(define-private (distribute-single-reward 
    (participant principal) 
    (data {census-id: uint, reward: uint}))
    (let ((census-id (get census-id data))
          (reward (get reward data))
          (response (map-get? census-responses {census-id: census-id, participant: participant}))
          (participant-data (map-get? participants participant)))
        (match response
            resp (if (get verified resp)
                (begin
                    (unwrap-panic (ft-mint? censix-token reward participant))
                    (match participant-data
                        part (map-set participants participant 
                            (merge part {rewards-earned: (+ (get rewards-earned part) reward)}))
                        true)
                    data)
                data)
            data)
    )
)

(define-public (claim-tokens (amount uint))
    (let ((participant (unwrap! (map-get? participants tx-sender) ERR_NOT_REGISTERED)))
        (asserts! (get active participant) ERR_NOT_REGISTERED)
        (asserts! (>= (ft-get-balance censix-token tx-sender) amount) ERR_INSUFFICIENT_BALANCE)
        (try! (ft-transfer? censix-token amount tx-sender CONTRACT_OWNER))
        (ok true)
    )
)

(define-public (update-registration-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set registration-fee new-fee)
        (ok true)
    )
)

(define-public (update-participation-reward (new-reward uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set participation-reward new-reward)
        (ok true)
    )
)

(define-public (deactivate-participant (participant principal))
    (let ((participant-data (unwrap! (map-get? participants participant) ERR_NOT_FOUND)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set participants participant 
            (merge participant-data {active: false}))
        (ok true)
    )
)

(define-public (emergency-stop-census (census-id uint))
    (let ((census (unwrap! (map-get? census-rounds census-id) ERR_NOT_FOUND)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set census-rounds census-id 
            (merge census {active: false}))
        (ok true)
    )
)

(define-read-only (get-participant-info (participant principal))
    (map-get? participants participant)
)

(define-read-only (get-census-info (census-id uint))
    (map-get? census-rounds census-id)
)

(define-read-only (get-census-question (census-id uint))
    (map-get? census-questions census-id)
)

(define-read-only (get-census-results (census-id uint))
    (map-get? census-results census-id)
)

(define-read-only (get-participant-response (census-id uint) (participant principal))
    (map-get? census-responses {census-id: census-id, participant: participant})
)

(define-read-only (has-responded (participant principal) (census-id uint))
    (is-some (map-get? participant-census-responses {participant: participant, census-id: census-id}))
)

(define-read-only (get-total-participants)
    (var-get total-participants)
)

(define-read-only (get-registration-fee)
    (var-get registration-fee)
)

(define-read-only (get-participation-reward)
    (var-get participation-reward)
)

(define-read-only (get-token-balance (account principal))
    (ft-get-balance censix-token account)
)

(define-read-only (get-current-census-id)
    (- (var-get next-census-id) u1)
)

(ft-mint? censix-token u1000000000 CONTRACT_OWNER)
