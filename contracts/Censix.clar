(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_CENSUS (err u103))
(define-constant ERR_CENSUS_CLOSED (err u104))
(define-constant ERR_ALREADY_RESPONDED (err u105))
(define-constant ERR_NOT_REGISTERED (err u106))
(define-constant ERR_INSUFFICIENT_BALANCE (err u107))
(define-constant ERR_REPUTATION_TOO_LOW (err u108))
(define-constant ERR_ACHIEVEMENT_NOT_FOUND (err u109))
(define-constant ERR_ALREADY_CLAIMED (err u110))

(define-fungible-token censix-token u1000000000)

(define-data-var next-census-id uint u1)
(define-data-var registration-fee uint u1000000)
(define-data-var participation-reward uint u500000)
(define-data-var total-participants uint u0)
(define-data-var next-achievement-id uint u1)

(define-map participants principal {
    registered-at: uint,
    total-responses: uint,
    rewards-earned: uint,
    active: bool,
    reputation-score: uint,
    verified-responses: uint,
    reputation-tier: uint,
    achievement-count: uint
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
    active: bool,
    min-reputation: uint
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

(define-map achievements uint {
    title: (string-ascii 64),
    description: (string-ascii 256),
    requirement-type: (string-ascii 32),
    requirement-value: uint,
    reward-multiplier: uint,
    active: bool
})

(define-map participant-achievements {participant: principal, achievement-id: uint} {
    earned-at: uint,
    claimed: bool
})

(define-map reputation-tiers uint {
    tier-name: (string-ascii 32),
    min-score: uint,
    max-score: uint,
    reward-multiplier: uint,
    badge-color: (string-ascii 16)
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
            active: true,
            reputation-score: u0,
            verified-responses: u0,
            reputation-tier: u0,
            achievement-count: u0
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
    (reward-pool uint)
    (min-reputation uint))
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
            active: true,
            min-reputation: min-reputation
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
        (asserts! (>= (get reputation-score participant) (get min-reputation census)) ERR_REPUTATION_TOO_LOW)
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
        (try! (update-reputation participant))
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
                    (let ((tier (get-participant-tier participant))
                          (multiplier (get reward-multiplier (unwrap-panic (map-get? reputation-tiers tier))))
                          (final-reward (* reward multiplier)))
                        (unwrap-panic (ft-mint? censix-token final-reward participant))
                        (match participant-data
                            part (map-set participants participant 
                                (merge part {rewards-earned: (+ (get rewards-earned part) final-reward)}))
                            true)
                        data))
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

(define-public (initialize-reputation-system)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set reputation-tiers u0 {
            tier-name: "Bronze",
            min-score: u0,
            max-score: u49,
            reward-multiplier: u1,
            badge-color: "bronze"
        })
        (map-set reputation-tiers u1 {
            tier-name: "Silver",
            min-score: u50,
            max-score: u99,
            reward-multiplier: u2,
            badge-color: "silver"
        })
        (map-set reputation-tiers u2 {
            tier-name: "Gold",
            min-score: u100,
            max-score: u199,
            reward-multiplier: u3,
            badge-color: "gold"
        })
        (map-set reputation-tiers u3 {
            tier-name: "Platinum",
            min-score: u200,
            max-score: u499,
            reward-multiplier: u4,
            badge-color: "platinum"
        })
        (map-set reputation-tiers u4 {
            tier-name: "Diamond",
            min-score: u500,
            max-score: u999999,
            reward-multiplier: u5,
            badge-color: "diamond"
        })
        (ok true)
    )
)

(define-public (create-achievement 
    (title (string-ascii 64))
    (description (string-ascii 256))
    (requirement-type (string-ascii 32))
    (requirement-value uint)
    (reward-multiplier uint))
    (let ((achievement-id (var-get next-achievement-id)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set achievements achievement-id {
            title: title,
            description: description,
            requirement-type: requirement-type,
            requirement-value: requirement-value,
            reward-multiplier: reward-multiplier,
            active: true
        })
        (var-set next-achievement-id (+ achievement-id u1))
        (ok achievement-id)
    )
)

(define-public (claim-achievement (achievement-id uint))
    (let ((achievement (unwrap! (map-get? achievements achievement-id) ERR_ACHIEVEMENT_NOT_FOUND))
          (participant (unwrap! (map-get? participants tx-sender) ERR_NOT_REGISTERED))
          (current-block stacks-block-height))
        (asserts! (get active achievement) ERR_ACHIEVEMENT_NOT_FOUND)
        (asserts! (is-none (map-get? participant-achievements {participant: tx-sender, achievement-id: achievement-id})) ERR_ALREADY_CLAIMED)
        (asserts! (check-achievement-requirement tx-sender achievement) ERR_REPUTATION_TOO_LOW)
        
        (map-set participant-achievements {participant: tx-sender, achievement-id: achievement-id} {
            earned-at: current-block,
            claimed: true
        })
        
        (map-set participants tx-sender 
            (merge participant {achievement-count: (+ (get achievement-count participant) u1)}))
        
        (ok true)
    )
)

(define-private (update-reputation (participant principal))
    (let ((participant-data (unwrap! (map-get? participants participant) ERR_NOT_REGISTERED))
          (new-verified-count (+ (get verified-responses participant-data) u1))
          (new-reputation-score (+ (get reputation-score participant-data) u10))
          (new-tier (calculate-reputation-tier new-reputation-score)))
        (map-set participants participant 
            (merge participant-data {
                verified-responses: new-verified-count,
                reputation-score: new-reputation-score,
                reputation-tier: new-tier
            }))
        (ok true)
    )
)

(define-private (calculate-reputation-tier (score uint))
    (if (>= score u500)
        u4
        (if (>= score u200)
            u3
            (if (>= score u100)
                u2
                (if (>= score u50)
                    u1
                    u0))))
)

(define-private (check-achievement-requirement (participant principal) (achievement {title: (string-ascii 64), description: (string-ascii 256), requirement-type: (string-ascii 32), requirement-value: uint, reward-multiplier: uint, active: bool}))
    (let ((participant-data (unwrap-panic (map-get? participants participant)))
          (req-type (get requirement-type achievement))
          (req-value (get requirement-value achievement)))
        (if (is-eq req-type "responses")
            (>= (get total-responses participant-data) req-value)
            (if (is-eq req-type "reputation")
                (>= (get reputation-score participant-data) req-value)
                (if (is-eq req-type "verified")
                    (>= (get verified-responses participant-data) req-value)
                    false)))
    )
)

(define-private (get-participant-tier (participant principal))
    (let ((participant-data (map-get? participants participant)))
        (match participant-data
            part (get reputation-tier part)
            u0)
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

(define-read-only (get-participant-reputation (participant principal))
    (let ((participant-data (map-get? participants participant)))
        (match participant-data
            part {
                reputation-score: (get reputation-score part),
                reputation-tier: (get reputation-tier part),
                verified-responses: (get verified-responses part),
                achievement-count: (get achievement-count part)
            }
            {
                reputation-score: u0,
                reputation-tier: u0,
                verified-responses: u0,
                achievement-count: u0
            })
    )
)

(define-read-only (get-reputation-tier-info (tier-id uint))
    (map-get? reputation-tiers tier-id)
)

(define-read-only (get-achievement-info (achievement-id uint))
    (map-get? achievements achievement-id)
)

(define-read-only (get-participant-achievement (participant principal) (achievement-id uint))
    (map-get? participant-achievements {participant: participant, achievement-id: achievement-id})
)

(define-read-only (check-reputation-requirement (participant principal) (min-reputation uint))
    (let ((participant-data (map-get? participants participant)))
        (match participant-data
            part (>= (get reputation-score part) min-reputation)
            false)
    )
)

(define-read-only (get-tier-multiplier (participant principal))
    (let ((tier (get-participant-tier participant))
          (tier-info (map-get? reputation-tiers tier)))
        (match tier-info
            info (get reward-multiplier info)
            u1)
    )
)

(ft-mint? censix-token u1000000000 CONTRACT_OWNER)
