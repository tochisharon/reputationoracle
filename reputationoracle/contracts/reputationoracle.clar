;; Reputation Oracle - Decentralized Trust and Reputation System
;; A production-ready smart contract for on-chain reputation management

;; Constants
(define-constant contract-owner tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-REGISTERED (err u101))
(define-constant ERR-NOT-REGISTERED (err u102))
(define-constant ERR-INVALID-PARAMS (err u200))
(define-constant ERR-INSUFFICIENT-STAKE (err u201))
(define-constant ERR-CATEGORY-NOT-FOUND (err u202))
(define-constant ERR-INVALID-RATING (err u203))
(define-constant ERR-COOLDOWN-ACTIVE (err u300))
(define-constant ERR-REVIEW-NOT-FOUND (err u301))
(define-constant ERR-ALREADY-REVIEWED (err u302))
(define-constant ERR-PAUSED (err u303))
(define-constant ERR-DISPUTE-EXPIRED (err u304))
(define-constant ERR-INSUFFICIENT-BALANCE (err u400))
(define-constant ERR-STAKE-LOCKED (err u401))
(define-constant ERR-SELF-REVIEW (err u500))
(define-constant ERR-ALREADY-CHALLENGED (err u501))
(define-constant ERR-NOT-CHALLENGER (err u502))

;; Data Variables
(define-data-var contract-paused bool false)
(define-data-var review-nonce uint u0)
(define-data-var dispute-nonce uint u0)
(define-data-var minimum-review-stake uint u10000000) ;; 10 STX
(define-data-var minimum-endorsement-stake uint u5000000) ;; 5 STX
(define-data-var challenge-stake-multiplier uint u2)
(define-data-var review-cooldown uint u144) ;; ~24 hours in blocks
(define-data-var slash-percentage uint u100) ;; 100% for proven false reviews
(define-data-var reward-rate uint u200) ;; 2% annual (basis points)
(define-data-var dispute-period uint u1440) ;; ~10 days to challenge
(define-data-var max-rating uint u100) ;; Rating scale 0-100
(define-data-var reputation-scale uint u10000) ;; Final score 0-10000

;; Data Maps

;; User Profiles
(define-map user-profiles
  principal
  {
    registered-at: uint,
    total-reviews-given: uint,
    total-reviews-received: uint,
    total-endorsements: uint,
    active-stake: uint,
    locked-stake: uint,
    reputation-score: uint,
    credibility-score: uint,
    is-active: bool
  }
)

;; Category Definitions
(define-map categories
  uint
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    is-active: bool,
    total-reviews: uint
  }
)

;; User Category Ratings
(define-map user-category-ratings
  { user: principal, category-id: uint }
  {
    total-score: uint,
    review-count: uint,
    weighted-average: uint,
    last-updated: uint
  }
)

;; Reviews
(define-map reviews
  uint
  {
    reviewer: principal,
    reviewee: principal,
    category-id: uint,
    rating: uint,
    stake-amount: uint,
    credibility-weight: uint,
    created-at: uint,
    is-disputed: bool,
    is-slashed: bool,
    metadata-hash: (optional (buff 32))
  }
)

;; Review Cooldowns
(define-map last-review-block
  { reviewer: principal, reviewee: principal }
  uint
)

;; Disputes
(define-map disputes
  uint
  {
    review-id: uint,
    challenger: principal,
    challenge-stake: uint,
    created-at: uint,
    resolved: bool,
    challenger-won: bool,
    evidence-hash: (optional (buff 32))
  }
)

;; Endorsements
(define-map endorsements
  { endorser: principal, endorsee: principal }
  {
    stake-amount: uint,
    created-at: uint,
    is-active: bool
  }
)

;; Rewards tracking
(define-map pending-rewards principal uint)

;; Private Functions

(define-private (is-paused)
  (var-get contract-paused)
)

(define-private (check-not-paused)
  (if (is-paused)
    ERR-PAUSED
    (ok true)
  )
)

(define-private (is-registered (user principal))
  (is-some (map-get? user-profiles user))
)

(define-private (increment-review-nonce)
  (let ((current (var-get review-nonce)))
    (var-set review-nonce (+ current u1))
    current
  )
)

(define-private (increment-dispute-nonce)
  (let ((current (var-get dispute-nonce)))
    (var-set dispute-nonce (+ current u1))
    current
  )
)

(define-private (min (a uint) (b uint))
  (if (< a b) a b)
)

(define-private (max (a uint) (b uint))
  (if (> a b) a b)
)

(define-private (abs-diff (a uint) (b uint))
  (if (> a b) (- a b) (- b a))
)

(define-private (calculate-credibility (profile {registered-at: uint, total-reviews-given: uint, total-reviews-received: uint, total-endorsements: uint, active-stake: uint, locked-stake: uint, reputation-score: uint, credibility-score: uint, is-active: bool}))
  (let
    (
      (age-factor (min u100 (/ (* (- stacks-block-height (get registered-at profile)) u100) u52560)))
      (stake-factor (min u100 (/ (get active-stake profile) u10000000)))
      (review-factor (min u100 (get total-reviews-given profile)))
      (base-credibility (/ (+ (+ age-factor stake-factor) review-factor) u3))
    )
    base-credibility
  )
)

;; Public Functions - User Management

(define-public (register-user (metadata-hash (optional (buff 32))))
  (begin
    (asserts! (not (is-registered tx-sender)) ERR-ALREADY-REGISTERED)
    (try! (check-not-paused))
    
    (map-set user-profiles tx-sender {
      registered-at: stacks-block-height,
      total-reviews-given: u0,
      total-reviews-received: u0,
      total-endorsements: u0,
      active-stake: u0,
      locked-stake: u0,
      reputation-score: u5000,
      credibility-score: u50,
      is-active: true
    })
    (ok true)
  )
)

(define-public (stake-for-reputation (amount uint))
  (let
    (
      (profile (unwrap! (map-get? user-profiles tx-sender) ERR-NOT-REGISTERED))
      (new-stake (+ (get active-stake profile) amount))
    )
    (asserts! (> amount u0) ERR-INVALID-PARAMS)
    (try! (check-not-paused))
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set user-profiles tx-sender (merge profile { active-stake: new-stake }))
    (ok new-stake)
  )
)

(define-public (unstake-reputation (amount uint))
  (let
    (
      (profile (unwrap! (map-get? user-profiles tx-sender) ERR-NOT-REGISTERED))
      (available-stake (get active-stake profile))
    )
    (asserts! (<= amount available-stake) ERR-INSUFFICIENT-STAKE)
    (asserts! (is-eq (get locked-stake profile) u0) ERR-STAKE-LOCKED)
    (try! (check-not-paused))
    
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    (map-set user-profiles tx-sender (merge profile { active-stake: (- available-stake amount) }))
    (ok true)
  )
)

;; Public Functions - Categories

(define-public (add-category (category-id uint) (name (string-ascii 50)) (description (string-ascii 200)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? categories category-id)) ERR-ALREADY-REGISTERED)
    
    (map-set categories category-id {
      name: name,
      description: description,
      is-active: true,
      total-reviews: u0
    })
    (ok true)
  )
)

(define-public (deactivate-category (category-id uint))
  (let
    (
      (category (unwrap! (map-get? categories category-id) ERR-CATEGORY-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender contract-owner) ERR-NOT-AUTHORIZED)
    (map-set categories category-id (merge category { is-active: false }))
    (ok true)
  )
)

;; Public Functions - Reviews

(define-public (submit-review 
  (reviewee principal) 
  (category-id uint) 
  (rating uint) 
  (stake-amount uint)
  (metadata-hash (optional (buff 32))))
  (let
    (
      (review-id (increment-review-nonce))
      (reviewer-profile (unwrap! (map-get? user-profiles tx-sender) ERR-NOT-REGISTERED))
      (reviewee-profile (unwrap! (map-get? user-profiles reviewee) ERR-NOT-REGISTERED))
      (category (unwrap! (map-get? categories category-id) ERR-CATEGORY-NOT-FOUND))
      (last-review (default-to u0 (map-get? last-review-block { reviewer: tx-sender, reviewee: reviewee })))
      (credibility (calculate-credibility reviewer-profile))
    )
    (asserts! (not (is-eq tx-sender reviewee)) ERR-SELF-REVIEW)
    (asserts! (>= stake-amount (var-get minimum-review-stake)) ERR-INSUFFICIENT-STAKE)
    (asserts! (and (>= rating u0) (<= rating (var-get max-rating))) ERR-INVALID-RATING)
    (asserts! (>= (- stacks-block-height last-review) (var-get review-cooldown)) ERR-COOLDOWN-ACTIVE)
    (asserts! (get is-active category) ERR-CATEGORY-NOT-FOUND)
    (try! (check-not-paused))
    
    ;; Transfer stake
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    
    ;; Create review
    (map-set reviews review-id {
      reviewer: tx-sender,
      reviewee: reviewee,
      category-id: category-id,
      rating: rating,
      stake-amount: stake-amount,
      credibility-weight: credibility,
      created-at: stacks-block-height,
      is-disputed: false,
      is-slashed: false,
      metadata-hash: metadata-hash
    })
    
    ;; Update reviewer profile
    (map-set user-profiles tx-sender 
      (merge reviewer-profile { 
        total-reviews-given: (+ (get total-reviews-given reviewer-profile) u1),
        locked-stake: (+ (get locked-stake reviewer-profile) stake-amount)
      })
    )
    
    ;; Update reviewee profile
    (map-set user-profiles reviewee
      (merge reviewee-profile {
        total-reviews-received: (+ (get total-reviews-received reviewee-profile) u1)
      })
    )
    
    ;; Update category rating
    (match (map-get? user-category-ratings { user: reviewee, category-id: category-id })
      existing-rating
        (let
          (
            (new-total (+ (get total-score existing-rating) rating))
            (new-count (+ (get review-count existing-rating) u1))
            (new-weighted (/ (* new-total u100) new-count))
          )
          (map-set user-category-ratings { user: reviewee, category-id: category-id }
            (merge existing-rating {
              total-score: new-total,
              review-count: new-count,
              weighted-average: new-weighted,
              last-updated: stacks-block-height
            })
          )
        )
      (map-set user-category-ratings { user: reviewee, category-id: category-id }
        {
          total-score: rating,
          review-count: u1,
          weighted-average: rating,
          last-updated: stacks-block-height
        }
      )
    )
    
    ;; Update cooldown
    (map-set last-review-block { reviewer: tx-sender, reviewee: reviewee } stacks-block-height)
    
    ;; Update category
    (map-set categories category-id
      (merge category { total-reviews: (+ (get total-reviews category) u1) })
    )
    
    (ok review-id)
  )
)

(define-public (endorse-user (endorsee principal) (stake-amount uint))
  (let
    (
      (endorser-profile (unwrap! (map-get? user-profiles tx-sender) ERR-NOT-REGISTERED))
      (endorsee-profile (unwrap! (map-get? user-profiles endorsee) ERR-NOT-REGISTERED))
      (existing-endorsement (map-get? endorsements { endorser: tx-sender, endorsee: endorsee }))
    )
    (asserts! (not (is-eq tx-sender endorsee)) ERR-SELF-REVIEW)
    (asserts! (>= stake-amount (var-get minimum-endorsement-stake)) ERR-INSUFFICIENT-STAKE)
    (asserts! (is-none existing-endorsement) ERR-ALREADY-REVIEWED)
    (try! (check-not-paused))
    
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    
    (map-set endorsements { endorser: tx-sender, endorsee: endorsee }
      {
        stake-amount: stake-amount,
        created-at: stacks-block-height,
        is-active: true
      }
    )
    
    (map-set user-profiles endorsee
      (merge endorsee-profile {
        total-endorsements: (+ (get total-endorsements endorsee-profile) u1)
      })
    )
    
    (ok true)
  )
)

(define-public (revoke-endorsement (endorsee principal))
  (let
    (
      (endorsement (unwrap! (map-get? endorsements { endorser: tx-sender, endorsee: endorsee }) ERR-REVIEW-NOT-FOUND))
      (endorsee-profile (unwrap! (map-get? user-profiles endorsee) ERR-NOT-REGISTERED))
    )
    (asserts! (get is-active endorsement) ERR-ALREADY-REVIEWED)
    (try! (check-not-paused))
    
    (try! (as-contract (stx-transfer? (get stake-amount endorsement) tx-sender tx-sender)))
    
    (map-set endorsements { endorser: tx-sender, endorsee: endorsee }
      (merge endorsement { is-active: false })
    )
    
    (map-set user-profiles endorsee
      (merge endorsee-profile {
        total-endorsements: (- (get total-endorsements endorsee-profile) u1)
      })
    )
    
    (ok true)
  )
)

;; Public Functions - Disputes

(define-public (challenge-review (review-id uint) (evidence-hash (optional (buff 32))))
  (let
    (
      (review (unwrap! (map-get? reviews review-id) ERR-REVIEW-NOT-FOUND))
      (dispute-id (increment-dispute-nonce))
      (challenge-stake (* (get stake-amount review) (var-get challenge-stake-multiplier)))
      (challenger-profile (unwrap! (map-get? user-profiles tx-sender) ERR-NOT-REGISTERED))
    )
    (asserts! (not (is-eq tx-sender (get reviewer review))) ERR-NOT-AUTHORIZED)
    (asserts! (not (get is-disputed review)) ERR-ALREADY-CHALLENGED)
    (asserts! (<= (- stacks-block-height (get created-at review)) (var-get dispute-period)) ERR-DISPUTE-EXPIRED)
    (try! (check-not-paused))
    
    (try! (stx-transfer? challenge-stake tx-sender (as-contract tx-sender)))
    
    (map-set disputes dispute-id {
      review-id: review-id,
      challenger: tx-sender,
      challenge-stake: challenge-stake,
      created-at: stacks-block-height,
      resolved: false,
      challenger-won: false,
      evidence-hash: evidence-hash
    })
    
    (map-set reviews review-id (merge review { is-disputed: true }))
    
    (ok dispute-id)
  )
)

(define-public (resolve-dispute (dispute-id uint) (challenger-wins bool))
  (let
    (
      (dispute (unwrap! (map-get? disputes dispute-id) ERR-REVIEW-NOT-FOUND))
      (review (unwrap! (map-get? reviews (get review-id dispute)) ERR-REVIEW-NOT-FOUND))
      (reviewer-profile (unwrap! (map-get? user-profiles (get reviewer review)) ERR-NOT-REGISTERED))
      (challenger-profile (unwrap! (map-get? user-profiles (get challenger dispute)) ERR-NOT-REGISTERED))
    )
    (asserts! (is-eq tx-sender contract-owner) ERR-NOT-AUTHORIZED)
    (asserts! (not (get resolved dispute)) ERR-ALREADY-CHALLENGED)
    (try! (check-not-paused))
    
    (if challenger-wins
      (begin
        ;; Slash reviewer stake and reward challenger
        (let
          (
            (slash-amount (get stake-amount review))
            (reward-amount (/ slash-amount u2))
          )
          (try! (as-contract (stx-transfer? (get challenge-stake dispute) tx-sender (get challenger dispute))))
          (try! (as-contract (stx-transfer? reward-amount tx-sender (get challenger dispute))))
          
          (map-set reviews (get review-id dispute) (merge review { is-slashed: true }))
          (map-set user-profiles (get reviewer review)
            (merge reviewer-profile {
              locked-stake: (- (get locked-stake reviewer-profile) slash-amount)
            })
          )
        )
      )
      (begin
        ;; Slash challenger stake and reward reviewer
        (let
          (
            (slash-amount (/ (get challenge-stake dispute) u2))
          )
          (try! (as-contract (stx-transfer? slash-amount tx-sender (get reviewer review))))
        )
      )
    )
    
    (map-set disputes dispute-id (merge dispute { resolved: true, challenger-won: challenger-wins }))
    (ok true)
  )
)

;; Public Functions - Rewards

(define-public (claim-reviewer-rewards)
  (let
    (
      (profile (unwrap! (map-get? user-profiles tx-sender) ERR-NOT-REGISTERED))
      (rewards (default-to u0 (map-get? pending-rewards tx-sender)))
    )
    (asserts! (> rewards u0) ERR-INSUFFICIENT-BALANCE)
    (try! (check-not-paused))
    
    (try! (as-contract (stx-transfer? rewards tx-sender tx-sender)))
    (map-delete pending-rewards tx-sender)
    (ok rewards)
  )
)

(define-public (unlock-review-stake (review-id uint))
  (let
    (
      (review (unwrap! (map-get? reviews review-id) ERR-REVIEW-NOT-FOUND))
      (reviewer-profile (unwrap! (map-get? user-profiles tx-sender) ERR-NOT-REGISTERED))
    )
    (asserts! (is-eq tx-sender (get reviewer review)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get is-disputed review)) ERR-ALREADY-CHALLENGED)
    (asserts! (> (- stacks-block-height (get created-at review)) (var-get dispute-period)) ERR-DISPUTE-EXPIRED)
    (asserts! (not (get is-slashed review)) ERR-INVALID-PARAMS)
    (try! (check-not-paused))
    
    (let
      (
        (stake-amount (get stake-amount review))
      )
      (try! (as-contract (stx-transfer? stake-amount tx-sender tx-sender)))
      (map-set user-profiles tx-sender
        (merge reviewer-profile {
          locked-stake: (- (get locked-stake reviewer-profile) stake-amount)
        })
      )
      (ok stake-amount)
    )
  )
)

;; Public Functions - Admin

(define-public (toggle-pause)
  (begin
    (asserts! (is-eq tx-sender contract-owner) ERR-NOT-AUTHORIZED)
    (var-set contract-paused (not (is-paused)))
    (ok (is-paused))
  )
)

(define-public (update-minimum-stakes (review-stake uint) (endorsement-stake uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) ERR-NOT-AUTHORIZED)
    (asserts! (> review-stake u0) ERR-INVALID-PARAMS)
    (asserts! (> endorsement-stake u0) ERR-INVALID-PARAMS)
    (var-set minimum-review-stake review-stake)
    (var-set minimum-endorsement-stake endorsement-stake)
    (ok true)
  )
)

(define-public (update-cooldown (new-cooldown uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) ERR-NOT-AUTHORIZED)
    (asserts! (> new-cooldown u0) ERR-INVALID-PARAMS)
    (var-set review-cooldown new-cooldown)
    (ok true)
  )
)

(define-public (update-dispute-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) ERR-NOT-AUTHORIZED)
    (asserts! (> new-period u0) ERR-INVALID-PARAMS)
    (var-set dispute-period new-period)
    (ok true)
  )
)

;; Read-Only Functions

(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles user)
)

(define-read-only (get-reputation-score (user principal))
  (match (map-get? user-profiles user)
    profile (ok (get reputation-score profile))
    ERR-NOT-REGISTERED
  )
)

(define-read-only (get-category-rating (user principal) (category-id uint))
  (map-get? user-category-ratings { user: user, category-id: category-id })
)

(define-read-only (get-review-details (review-id uint))
  (map-get? reviews review-id)
)

(define-read-only (get-category-info (category-id uint))
  (map-get? categories category-id)
)

(define-read-only (get-endorsement (endorser principal) (endorsee principal))
  (map-get? endorsements { endorser: endorser, endorsee: endorsee })
)

(define-read-only (get-dispute-details (dispute-id uint))
  (map-get? disputes dispute-id)
)

(define-read-only (get-pending-rewards (user principal))
  (ok (default-to u0 (map-get? pending-rewards user)))
)

(define-read-only (get-reviewer-credibility (reviewer principal))
  (match (map-get? user-profiles reviewer)
    profile (ok (calculate-credibility profile))
    ERR-NOT-REGISTERED
  )
)

(define-read-only (is-user-registered (user principal))
  (ok (is-registered user))
)

(define-read-only (get-contract-balance)
  (ok (stx-get-balance (as-contract tx-sender)))
)

(define-read-only (get-contract-status)
  (ok {
    paused: (is-paused),
    total-reviews: (var-get review-nonce),
    total-disputes: (var-get dispute-nonce),
    min-review-stake: (var-get minimum-review-stake),
    min-endorsement-stake: (var-get minimum-endorsement-stake),
    review-cooldown: (var-get review-cooldown),
    dispute-period: (var-get dispute-period)
  })
)

(define-read-only (get-review-cooldown-status (reviewer principal) (reviewee principal))
  (let
    (
      (last-review (default-to u0 (map-get? last-review-block { reviewer: reviewer, reviewee: reviewee })))
      (blocks-since-review (- stacks-block-height last-review))
      (cooldown-remaining (if (>= blocks-since-review (var-get review-cooldown)) 
        u0 
        (- (var-get review-cooldown) blocks-since-review)))
    )
    (ok {
      can-review: (>= blocks-since-review (var-get review-cooldown)),
      blocks-remaining: cooldown-remaining,
      last-review-block: last-review
    })
  )
)