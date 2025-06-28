# Censix - Digital Census Token

A decentralized census platform built on the Stacks blockchain that enables community-driven data collection with cryptographic privacy and token-based incentives.

## Overview

Censix allows communities to conduct transparent, verifiable census operations on-chain. Participants register, contribute to census rounds, and earn CENSIX tokens for verified responses. The contract ensures data integrity while maintaining participant privacy through cryptographic hashing.

## Features

- **Participant Registration**: Pay registration fee to join the census network
- **Census Creation**: Admins can create time-bound census rounds with custom questions
- **Anonymous Responses**: Submit responses as cryptographic hashes for privacy
- **Token Rewards**: Earn CENSIX tokens for verified participation
- **Data Verification**: Admin verification system for response authenticity
- **Result Finalization**: Aggregate and publish census results on-chain

## Contract Functions

### Registration
- `register-participant()` - Register as a census participant (requires fee)
- `get-participant-info(principal)` - View participant details

### Census Management
- `create-census(title, description, duration, min-participants, reward-pool)` - Create new census
- `add-census-question(census-id, question, options)` - Add questions to census
- `emergency-stop-census(census-id)` - Stop active census (admin only)

### Participation
- `submit-census-response(census-id, response-hash)` - Submit encrypted census response
- `has-responded(participant, census-id)` - Check if user already responded

### Verification & Rewards
- `verify-response(census-id, participant)` - Verify participant response (admin only)
- `distribute-rewards(census-id, participants-list)` - Distribute tokens to verified participants
- `finalize-census(census-id, result-hash)` - Finalize census with aggregated results

### Token Operations
- `claim-tokens(amount)` - Transfer earned tokens back to contract
- `get-token-balance(account)` - Check CENSIX token balance

## Usage Example

```clarity
;; 1. Register as participant
(contract-call? .censix register-participant)

;; 2. Create census (admin only)
(contract-call? .censix create-census 
    "Community Health Survey" 
    "Annual health and wellness census" 
    u1440 ;; ~10 days
    u100  ;; minimum 100 participants
    u50000000) ;; reward pool

;; 3. Add census question (admin only)
(contract-call? .censix add-census-question 
    u1 
    "What is your age group?" 
    (list "18-25" "26-35" "36-45" "46-55" "55+"))

;; 4. Submit response (participants)
(contract-call? .censix submit-census-response 
    u1 
    0x1234567890abcdef1234567890abcdef12345678)

;; 5. Verify responses (admin only)
(contract-call? .censix verify-response u1 'SP1PARTICIPANT...)

;; 6. Finalize census (admin only)
(contract-call? .censix finalize-census 
    u1 
    0xabcdef1234567890abcdef1234567890abcdef12)
```

## Token Economics

- **Registration Fee**: Default 1 STX (adjustable by admin)
- **Participation Rewards**: Distributed from census reward pools
- **Total Supply**: 1 billion CENSIX tokens
- **Reward Distribution**: Equal split among verified participants

## Privacy Model

Responses are submitted as SHA-256 hashes, ensuring:
- Response privacy until results publication
- Tamper-proof submissions
- Verifiable participation without exposing individual answers
- Aggregate result integrity

## Admin Functions

Contract owner can:
- Create and manage census rounds
- Verify participant responses
- Adjust registration fees and rewards
- Emergency stop active census
- Deactivate problematic participants

## Error Codes

- `u100`: Unauthorized access
- `u101`: Resource not found
- `u102`: Resource already exists
- `u103`: Invalid census parameters
- `u104`: Census is closed
- `u105`: Already responded to census
- `u106`: Not registered as participant
- `u107`: Insufficient token balance

## Development

Built with Clarinet and Clarity v2. Requires Stacks 2.1+ for deployment.

### Testing
```bash
clarinet test
```

### Deployment
```bash
clarinet deploy
```
