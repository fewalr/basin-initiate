;; Basin-Initiate DAO
;; A decentralized governance platform with multi-dimensional reputation.
;; Members earn reputation in distinct domains and use it to vote on proposals
;; that are weighted by domain expertise.

;; Constants
(define-constant CONTRACT-OWNER tx-sender)

(define-constant ERR-NOT-FOUND          (err u100))
(define-constant ERR-UNAUTHORIZED       (err u101))
(define-constant ERR-ALREADY-MEMBER     (err u102))
(define-constant ERR-INVALID-DOMAIN     (err u103))
(define-constant ERR-PROPOSAL-CLOSED    (err u104))
(define-constant ERR-ALREADY-VOTED      (err u105))
(define-constant ERR-INSUFFICIENT-REP   (err u106))
(define-constant ERR-INVALID-STAKE      (err u107))
(define-constant ERR-PROPOSAL-ACTIVE    (err u108))
(define-constant ERR-STAKE-LOCKED       (err u109))

;; Domain IDs
;; 0 = technical, 1 = community, 2 = treasury, 3 = strategy
(define-constant DOMAIN-TECHNICAL  u0)
(define-constant DOMAIN-COMMUNITY  u1)
(define-constant DOMAIN-TREASURY   u2)
(define-constant DOMAIN-STRATEGY   u3)
(define-constant DOMAIN-COUNT      u4)

;; Governance parameters
(define-constant MIN-REP-TO-PROPOSE     u100)
(define-constant VOTING-PERIOD-BLOCKS   u1440)  ;; ~10 days at 10-min blocks
(define-constant QUORUM-THRESHOLD       u500)   ;; total weighted votes needed
(define-constant PASS-THRESHOLD-PCT     u60)    ;; percent of weighted yes votes

;; =============================================================================
;; Data Maps and Vars
;; =============================================================================

;; Tracks whether an address is a registered member
(define-map members
  { member: principal }
  { joined-at: uint, active: bool }
)

;; Reputation per member per domain
;; key: { member, domain } value: { score }
(define-map reputation
  { member: principal, domain: uint }
  { score: uint }
)

;; Staked reputation: member locks rep to temporarily boost voting power
;; Stake is released after the proposal closes
(define-map reputation-stakes
  { member: principal, proposal-id: uint }
  { domain: uint, amount: uint, released: bool }
)

;; Peer recognition: member can endorse another member in a domain once
(define-map endorsements
  { from: principal, to: principal, domain: uint }
  { given: bool }
)

;; Proposal storage
(define-map proposals
  { proposal-id: uint }
  {
    proposer:       principal,
    domain:         uint,
    title:          (string-ascii 128),
    description:    (string-ascii 512),
    start-block:    uint,
    end-block:      uint,
    yes-weight:     uint,
    no-weight:      uint,
    executed:       bool,
    passed:         bool
  }
)

;; Vote records to prevent double voting
(define-map votes
  { voter: principal, proposal-id: uint }
  { voted: bool, support: bool, weight: uint }
)

;; Global counters
(define-data-var proposal-count uint u0)
(define-data-var member-count   uint u0)

;; =============================================================================
;; Private helpers
;; =============================================================================

;; Get reputation score for a member in a domain, defaulting to 0
(define-private (get-rep (member principal) (domain uint))
  (default-to u0
    (get score (map-get? reputation { member: member, domain: domain }))
  )
)

;; Check domain is within valid range
(define-private (valid-domain (domain uint))
  (< domain DOMAIN-COUNT)
)

;; Compute voting weight: base reputation + any stake for this proposal
(define-private (compute-weight (voter principal) (domain uint) (proposal-id uint))
  (let (
    (base-rep  (get-rep voter domain))
    (stake-amt (default-to u0
                 (get amount
                   (map-get? reputation-stakes { member: voter, proposal-id: proposal-id })
                 )
               ))
  )
    (+ base-rep stake-amt)
  )
)

;; =============================================================================
;; Member Management
;; =============================================================================

;; Register as a DAO member. Anyone may join; initial reputation is zero.
(define-public (register-member)
  (let ((caller tx-sender))
    (asserts! (is-none (map-get? members { member: caller })) ERR-ALREADY-MEMBER)
    (map-set members
      { member: caller }
      { joined-at: block-height, active: true }
    )
    (var-set member-count (+ (var-get member-count) u1))
    (ok true)
  )
)

;; =============================================================================
;; Reputation
;; =============================================================================

;; Owner can grant reputation to a member in a domain (e.g., after off-chain review)
(define-public (grant-reputation (member principal) (domain uint) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (valid-domain domain) ERR-INVALID-DOMAIN)
    (asserts! (is-some (map-get? members { member: member })) ERR-NOT-FOUND)
    (map-set reputation
      { member: member, domain: domain }
      { score: (+ (get-rep member domain) amount) }
    )
    (ok true)
  )
)

;; Peer endorsement: adds a small fixed reputation bonus to the recipient.
;; Each address can endorse another address in a given domain only once.
(define-public (endorse-member (recipient principal) (domain uint))
  (let ((caller tx-sender))
    (asserts! (not (is-eq caller recipient))            ERR-UNAUTHORIZED)
    (asserts! (valid-domain domain)                    ERR-INVALID-DOMAIN)
    (asserts! (is-some (map-get? members { member: caller }))     ERR-NOT-FOUND)
    (asserts! (is-some (map-get? members { member: recipient }))  ERR-NOT-FOUND)
    (asserts!
      (is-none (map-get? endorsements { from: caller, to: recipient, domain: domain }))
      ERR-ALREADY-VOTED
    )
    (map-set endorsements
      { from: caller, to: recipient, domain: domain }
      { given: true }
    )
    ;; Each endorsement grants 5 reputation points
    (map-set reputation
      { member: recipient, domain: domain }
      { score: (+ (get-rep recipient domain) u5) }
    )
    (ok true)
  )
)

;; =============================================================================
;; Proposals
;; =============================================================================

;; Create a proposal in a given domain. Proposer must have minimum reputation.
(define-public (create-proposal
    (domain uint)
    (title (string-ascii 128))
    (description (string-ascii 512))
  )
  (let (
    (caller      tx-sender)
    (pid         (+ (var-get proposal-count) u1))
    (caller-rep  (get-rep caller domain))
  )
    (asserts! (is-some (map-get? members { member: caller })) ERR-NOT-FOUND)
    (asserts! (valid-domain domain)                          ERR-INVALID-DOMAIN)
    (asserts! (>= caller-rep MIN-REP-TO-PROPOSE)             ERR-INSUFFICIENT-REP)
    (map-set proposals
      { proposal-id: pid }
      {
        proposer:    caller,
        domain:      domain,
        title:       title,
        description: description,
        start-block: block-height,
        end-block:   (+ block-height VOTING-PERIOD-BLOCKS),
        yes-weight:  u0,
        no-weight:   u0,
        executed:    false,
        passed:      false
      }
    )
    (var-set proposal-count pid)
    (ok pid)
  )
)

;; =============================================================================
;; Reputation Staking
;; =============================================================================

;; Lock reputation points to boost voting power on a specific proposal.
;; Staked amount is deducted from base reputation until released.
(define-public (stake-reputation (proposal-id uint) (domain uint) (amount uint))
  (let (
    (caller   tx-sender)
    (base-rep (get-rep caller domain))
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NOT-FOUND))
  )
    (asserts! (is-some (map-get? members { member: caller })) ERR-NOT-FOUND)
    (asserts! (valid-domain domain)                          ERR-INVALID-DOMAIN)
    (asserts! (> amount u0)                                  ERR-INVALID-STAKE)
    (asserts! (>= base-rep amount)                           ERR-INSUFFICIENT-REP)
    ;; Proposal must still be open
    (asserts! (<= block-height (get end-block proposal))     ERR-PROPOSAL-CLOSED)
    ;; Only one stake entry per member per proposal
    (asserts!
      (is-none (map-get? reputation-stakes { member: caller, proposal-id: proposal-id }))
      ERR-ALREADY-VOTED
    )
    ;; Deduct from base reputation
    (map-set reputation
      { member: caller, domain: domain }
      { score: (- base-rep amount) }
    )
    ;; Record stake
    (map-set reputation-stakes
      { member: caller, proposal-id: proposal-id }
      { domain: domain, amount: amount, released: false }
    )
    (ok true)
  )
)

;; Release staked reputation after a proposal has closed
(define-public (release-stake (proposal-id uint))
  (let (
    (caller  tx-sender)
    (stake   (unwrap! (map-get? reputation-stakes { member: caller, proposal-id: proposal-id }) ERR-NOT-FOUND))
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NOT-FOUND))
  )
    (asserts! (not (get released stake))                 ERR-STAKE-LOCKED)
    ;; Proposal must be closed before releasing
    (asserts! (> block-height (get end-block proposal))  ERR-PROPOSAL-ACTIVE)
    ;; Return staked amount to base reputation
    (map-set reputation
      { member: caller, domain: (get domain stake) }
      { score: (+ (get-rep caller (get domain stake)) (get amount stake)) }
    )
    (map-set reputation-stakes
      { member: caller, proposal-id: proposal-id }
      (merge stake { released: true })
    )
    (ok true)
  )
)

;; =============================================================================
;; Voting
;; =============================================================================

;; Cast a weighted vote on an open proposal.
;; Weight is calculated from the voter's reputation in the proposal's domain
;; plus any staked reputation for this proposal.
(define-public (cast-vote (proposal-id uint) (support bool))
  (let (
    (caller   tx-sender)
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NOT-FOUND))
    (domain   (get domain proposal))
    (weight   (compute-weight caller domain proposal-id))
  )
    (asserts! (is-some (map-get? members { member: caller })) ERR-NOT-FOUND)
    ;; Proposal must be in the active voting window
    (asserts! (>= block-height (get start-block proposal))    ERR-PROPOSAL-CLOSED)
    (asserts! (<= block-height (get end-block proposal))      ERR-PROPOSAL-CLOSED)
    ;; No double voting
    (asserts!
      (is-none (map-get? votes { voter: caller, proposal-id: proposal-id }))
      ERR-ALREADY-VOTED
    )
    (asserts! (> weight u0) ERR-INSUFFICIENT-REP)
    ;; Record vote
    (map-set votes
      { voter: caller, proposal-id: proposal-id }
      { voted: true, support: support, weight: weight }
    )
    ;; Update proposal tallies
    (if support
      (map-set proposals { proposal-id: proposal-id }
        (merge proposal { yes-weight: (+ (get yes-weight proposal) weight) })
      )
      (map-set proposals { proposal-id: proposal-id }
        (merge proposal { no-weight: (+ (get no-weight proposal) weight) })
      )
    )
    (ok weight)
  )
)

;; =============================================================================
;; Finalize Proposal
;; =============================================================================

;; Anyone can call this after the voting period to record the outcome.
;; Passes if quorum is met and yes-weight >= PASS-THRESHOLD-PCT of total weight.
(define-public (finalize-proposal (proposal-id uint))
  (let (
    (proposal    (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NOT-FOUND))
    (yes-w       (get yes-weight proposal))
    (no-w        (get no-weight  proposal))
    (total-w     (+ yes-w no-w))
    (quorum-met  (>= total-w QUORUM-THRESHOLD))
    (threshold-met
      (if (> total-w u0)
        (>= (* yes-w u100) (* total-w PASS-THRESHOLD-PCT))
        false
      )
    )
    (did-pass    (and quorum-met threshold-met))
  )
    ;; Voting period must have ended
    (asserts! (> block-height (get end-block proposal)) ERR-PROPOSAL-ACTIVE)
    ;; Not already finalized
    (asserts! (not (get executed proposal)) ERR-PROPOSAL-CLOSED)
    (map-set proposals { proposal-id: proposal-id }
      (merge proposal { executed: true, passed: did-pass })
    )
    (ok did-pass)
  )
)

;; =============================================================================
;; Read-Only Functions
;; =============================================================================

(define-read-only (get-member-info (member principal))
  (map-get? members { member: member })
)

(define-read-only (get-reputation (member principal) (domain uint))
  (get-rep member domain)
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-vote (voter principal) (proposal-id uint))
  (map-get? votes { voter: voter, proposal-id: proposal-id })
)

(define-read-only (get-stake (member principal) (proposal-id uint))
  (map-get? reputation-stakes { member: member, proposal-id: proposal-id })
)

(define-read-only (get-proposal-count)
  (var-get proposal-count)
)

(define-read-only (get-member-count)
  (var-get member-count)
)

(define-read-only (get-voting-weight (voter principal) (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal (ok (compute-weight voter (get domain proposal) proposal-id))
    ERR-NOT-FOUND
  )
)
