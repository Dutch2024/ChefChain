;; ChefChain: Culinary Skills Development Platform
;; Version: 1.0.0

;; Constants
(define-constant KITCHEN_CAPACITY u3200000)
(define-constant BASE_CULINARY_REWARD u40)
(define-constant TECHNIQUE_BONUS u22)
(define-constant MAX_CHEF_LEVEL u24)
(define-constant ERR_INVALID_COOKING_SESSION u1)
(define-constant ERR_NO_CULINARY_TOKENS u2)
(define-constant ERR_KITCHEN_CAPACITY_EXCEEDED u3)
(define-constant BLOCKS_PER_CULINARY_CYCLE u2448)
(define-constant EQUIPMENT_INVESTMENT_MULTIPLIER u14)
(define-constant MIN_INVESTMENT_PERIOD u1224)
(define-constant EARLY_WITHDRAWAL_PENALTY u30)

;; Data Variables
(define-data-var total-culinary-tokens-distributed uint u0)
(define-data-var total-cooking-sessions uint u0)
(define-data-var head-chef principal tx-sender)

;; Data Maps
(define-map chef-cooking-sessions principal uint)
(define-map chef-culinary-tokens principal uint)
(define-map cooking-session-start-time principal uint)
(define-map chef-skill-level principal uint)
(define-map chef-last-cooking-session principal uint)
(define-map chef-equipment-investments principal uint)
(define-map chef-investment-start-block principal uint)
(define-map cuisine-type-specialization principal uint)
(define-map chef-signature-dishes principal uint)
(define-map culinary-technique-mastery principal uint)

;; Public Functions
(define-public (start-cooking-session (cuisine-type uint) (recipe-difficulty uint))
  (let
    (
      (chef tx-sender)
    )
    (asserts! (and (> cuisine-type u0) (> recipe-difficulty u0) (<= recipe-difficulty u100)) (err ERR_INVALID_COOKING_SESSION))
    (map-set cooking-session-start-time chef burn-block-height)
    (map-set cuisine-type-specialization chef cuisine-type)
    (ok true)
  ))

(define-public (complete-cooking-session (recipe-difficulty uint) (presentation-score uint))
  (let
    (
      (chef tx-sender)
      (start-block (default-to u0 (map-get? cooking-session-start-time chef)))
      (blocks-cooking (- burn-block-height start-block))
      (last-session-block (default-to u0 (map-get? chef-last-cooking-session chef)))
      (skill-level (default-to u0 (map-get? chef-skill-level chef)))
      (capped-skill (if (<= skill-level MAX_CHEF_LEVEL) skill-level MAX_CHEF_LEVEL))
      (technique-bonus (default-to u0 (map-get? culinary-technique-mastery chef)))
      (presentation-bonus (/ (* presentation-score u20) u100))
      (difficulty-bonus (/ recipe-difficulty u4))
      (cooking-reward (+ BASE_CULINARY_REWARD (* capped-skill TECHNIQUE_BONUS) technique-bonus presentation-bonus difficulty-bonus))
    )
    (asserts! (and (> start-block u0) (>= blocks-cooking (/ recipe-difficulty u25)) (<= presentation-score u100)) (err ERR_INVALID_COOKING_SESSION))
    
    (map-set chef-cooking-sessions chef (+ (default-to u0 (map-get? chef-cooking-sessions chef)) u1))
    (map-set chef-culinary-tokens chef (+ (default-to u0 (map-get? chef-culinary-tokens chef)) cooking-reward))
    
    (if (< (- burn-block-height last-session-block) BLOCKS_PER_CULINARY_CYCLE)
      (map-set chef-skill-level chef (+ skill-level u1))
      (map-set chef-skill-level chef u1)
    )
    
    (if (>= presentation-score u92)
      (begin
        (map-set chef-signature-dishes chef (+ (default-to u0 (map-get? chef-signature-dishes chef)) u1))
        (map-set culinary-technique-mastery chef (+ technique-bonus u14))
      )
      true
    )
    
    (map-set chef-last-cooking-session chef burn-block-height)
    (var-set total-cooking-sessions (+ (var-get total-cooking-sessions) u1))
    (var-set total-culinary-tokens-distributed (+ (var-get total-culinary-tokens-distributed) cooking-reward))
    
    (asserts! (<= (var-get total-culinary-tokens-distributed) KITCHEN_CAPACITY) (err ERR_KITCHEN_CAPACITY_EXCEEDED))
    (ok cooking-reward)
  ))

(define-public (claim-culinary-rewards)
  (let
    (
      (chef tx-sender)
      (token-balance (default-to u0 (map-get? chef-culinary-tokens chef)))
    )
    (asserts! (> token-balance u0) (err ERR_NO_CULINARY_TOKENS))
    (map-set chef-culinary-tokens chef u0)
    (ok token-balance)
  ))

;; Kitchen Equipment Investment Features
(define-public (invest-in-kitchen-equipment (amount uint))
  (let
    (
      (chef tx-sender)
    )
    (asserts! (> amount u0) (err ERR_INVALID_COOKING_SESSION))
    (asserts! (>= (var-get total-culinary-tokens-distributed) amount) (err ERR_KITCHEN_CAPACITY_EXCEEDED))
    
    (map-set chef-equipment-investments chef amount)
    (map-set chef-investment-start-block chef burn-block-height)
    (var-set total-culinary-tokens-distributed (- (var-get total-culinary-tokens-distributed) amount))
    (ok amount)
  ))

(define-public (withdraw-equipment-investment)
  (let
    (
      (chef tx-sender)
      (invested-amount (default-to u0 (map-get? chef-equipment-investments chef)))
      (investment-start-block (default-to u0 (map-get? chef-investment-start-block chef)))
      (blocks-invested (- burn-block-height investment-start-block))
      (penalty (if (< blocks-invested MIN_INVESTMENT_PERIOD) (/ (* invested-amount EARLY_WITHDRAWAL_PENALTY) u100) u0))
      (investment-bonus (if (>= blocks-invested MIN_INVESTMENT_PERIOD) (/ (* invested-amount EQUIPMENT_INVESTMENT_MULTIPLIER) u100) u0))
      (final-amount (+ (- invested-amount penalty) investment-bonus))
    )
    (asserts! (> invested-amount u0) (err ERR_NO_CULINARY_TOKENS))
    
    (map-set chef-equipment-investments chef u0)
    (map-set chef-investment-start-block chef u0)
    (var-set total-culinary-tokens-distributed (+ (var-get total-culinary-tokens-distributed) final-amount))
    (ok final-amount)
  ))

(define-public (publish-cookbook (cookbook-title (string-utf8 128)) (recipe-count uint))
  (let
    (
      (chef tx-sender)
      (skill-level (default-to u0 (map-get? chef-skill-level chef)))
      (signature-dishes (default-to u0 (map-get? chef-signature-dishes chef)))
      (cookbook-bonus (+ (* recipe-count u70) (* signature-dishes u35) (* skill-level u28)))
    )
    (asserts! (and (> (len cookbook-title) u0) (>= skill-level u18) (> recipe-count u0)) (err ERR_INVALID_COOKING_SESSION))
    
    (map-set chef-culinary-tokens chef (+ (default-to u0 (map-get? chef-culinary-tokens chef)) cookbook-bonus))
    (var-set total-culinary-tokens-distributed (+ (var-get total-culinary-tokens-distributed) cookbook-bonus))
    
    (ok cookbook-bonus)
  ))

(define-public (host-cooking-workshop (participant-count uint) (workshop-hours uint))
  (let
    (
      (chef tx-sender)
      (skill-level (default-to u0 (map-get? chef-skill-level chef)))
      (technique-mastery (default-to u0 (map-get? culinary-technique-mastery chef)))
      (workshop-bonus (+ (* participant-count u50) (* workshop-hours u25) (* technique-mastery u10)))
    )
    (asserts! (and (> participant-count u0) (> workshop-hours u0) (>= skill-level u22)) (err ERR_INVALID_COOKING_SESSION))
    
    (map-set chef-culinary-tokens chef (+ (default-to u0 (map-get? chef-culinary-tokens chef)) workshop-bonus))
    (var-set total-culinary-tokens-distributed (+ (var-get total-culinary-tokens-distributed) workshop-bonus))
    
    (ok workshop-bonus)
  ))

(define-public (enter-cooking-competition (competition-level uint) (entry-fee uint))
  (let
    (
      (chef tx-sender)
      (skill-level (default-to u0 (map-get? chef-skill-level chef)))
      (signature-dishes (default-to u0 (map-get? chef-signature-dishes chef)))
      (competition-bonus (+ (* competition-level u60) (* signature-dishes u20)))
    )
    (asserts! (and (> competition-level u0) (>= skill-level u14) (> entry-fee u0)) (err ERR_INVALID_COOKING_SESSION))
    (asserts! (>= (var-get total-culinary-tokens-distributed) entry-fee) (err ERR_KITCHEN_CAPACITY_EXCEEDED))
    
    (map-set chef-culinary-tokens chef (+ (default-to u0 (map-get? chef-culinary-tokens chef)) competition-bonus))
    (var-set total-culinary-tokens-distributed (+ (- (var-get total-culinary-tokens-distributed) entry-fee) competition-bonus))
    
    (ok competition-bonus)
  ))

;; Read-Only Functions
(define-read-only (get-cooking-session-count (user principal))
  (default-to u0 (map-get? chef-cooking-sessions user)))

(define-read-only (get-culinary-token-balance (user principal))
  (default-to u0 (map-get? chef-culinary-tokens user)))

(define-read-only (get-chef-skill-level (user principal))
  (default-to u0 (map-get? chef-skill-level user)))

(define-read-only (get-signature-dishes (user principal))
  (default-to u0 (map-get? chef-signature-dishes user)))

(define-read-only (get-equipment-investments (user principal))
  (default-to u0 (map-get? chef-equipment-investments user)))

(define-read-only (get-technique-mastery (user principal))
  (default-to u0 (map-get? culinary-technique-mastery user)))

(define-read-only (get-kitchen-stats)
  {
    total-cooking-sessions: (var-get total-cooking-sessions),
    total-culinary-tokens-distributed: (var-get total-culinary-tokens-distributed),
    kitchen-capacity: KITCHEN_CAPACITY
  })

(define-read-only (calculate-cooking-reward (skill-level uint) (presentation-score uint) (technique-bonus uint) (difficulty uint))
  (let
    (
      (capped-skill (if (<= skill-level MAX_CHEF_LEVEL) skill-level MAX_CHEF_LEVEL))
      (presentation-bonus (/ (* presentation-score u20) u100))
      (difficulty-bonus (/ difficulty u4))
    )
    (+ BASE_CULINARY_REWARD (* capped-skill TECHNIQUE_BONUS) technique-bonus presentation-bonus difficulty-bonus)
  ))

;; Private Functions
(define-private (is-head-chef)
  (is-eq tx-sender (var-get head-chef)))

(define-private (validate-cooking-parameters (recipe-difficulty uint) (presentation-score uint))
  (and (> recipe-difficulty u0) (<= presentation-score u100)))