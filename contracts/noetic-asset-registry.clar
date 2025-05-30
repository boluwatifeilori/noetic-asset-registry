;; Noetic Asset Management System

;; =============================================================================
;; |                             CONTRACT CONSTANTS                            |
;; =============================================================================

;; System administrator designation
(define-constant CONTRACT_SUPERVISOR tx-sender)

;; Error code definitions
(define-constant ERROR_ACCESS_DENIED (err u300))
(define-constant ERROR_ENTRY_NONEXISTENT (err u301))
(define-constant ERROR_DUPLICATE_SUBMISSION (err u302))
(define-constant ERROR_IMPROPER_DESCRIPTOR (err u303))
(define-constant ERROR_DIMENSIONS_INVALID (err u304))
(define-constant ERROR_PERMISSION_INSUFFICIENT (err u305))

;; =============================================================================
;; |                          STATE VARIABLE MANAGEMENT                         |
;; =============================================================================

;; Sequential identifier tracker
(define-data-var entry-counter uint u0)

;; =============================================================================
;; |                          PRIMARY DATA STRUCTURES                           |
;; =============================================================================

;; Scholarly work registry
(define-map scholarly-entries
  { entry-id: uint }
  {
    work-descriptor: (string-ascii 80),
    scholar-identity: principal,
    work-magnitude: uint,
    submission-block: uint,
    work-synopsis: (string-ascii 256),
    classification-tags: (list 8 (string-ascii 40))
  }
)

;; Permission management system
(define-map viewing-permissions
  { entry-id: uint, viewer: principal }
  { permission-granted: bool }
)

;; =============================================================================
;; |                           AUXILIARY FUNCTIONS                              |
;; =============================================================================

;; Validates if an entry exists within the repository
(define-private (entry-exists (entry-id uint))
  (is-some (map-get? scholarly-entries { entry-id: entry-id }))
)

;; Comprehensive tag validation system
(define-private (validate-classification-set (tags (list 8 (string-ascii 40))))
  (and
    (> (len tags) u0)
    (<= (len tags) u8)
    (is-eq (len (filter validate-single-tag tags)) (len tags))
  )
)
