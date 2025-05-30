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

;; Individual tag validation processor
(define-private (validate-single-tag (tag (string-ascii 40)))
  (and 
    (> (len tag) u0)
    (< (len tag) u41)
  )
)

;; Ownership verification protocol
(define-private (is-entry-owner (entry-id uint) (claimant principal))
  (match (map-get? scholarly-entries { entry-id: entry-id })
    entry-data (is-eq (get scholar-identity entry-data) claimant)
    false
  )
)

;; Work size extraction utility
(define-private (extract-work-size (entry-id uint))
  (default-to u0 
    (get work-magnitude 
      (map-get? scholarly-entries { entry-id: entry-id })
    )
  )
)

;; =============================================================================
;; |                        WORK SUBMISSION FUNCTIONS                           |
;; =============================================================================

;; Documents a new scholarly contribution in the repository
(define-public (submit-scholarly-work (descriptor (string-ascii 80)) (magnitude uint) (synopsis (string-ascii 256)) (tags (list 8 (string-ascii 40))))
  (let
    (
      (entry-id (+ (var-get entry-counter) u1))
    )
    ;; Validate submission parameters
    (asserts! (> (len descriptor) u0) ERROR_IMPROPER_DESCRIPTOR)
    (asserts! (< (len descriptor) u81) ERROR_IMPROPER_DESCRIPTOR)
    (asserts! (> magnitude u0) ERROR_DIMENSIONS_INVALID)
    (asserts! (< magnitude u2000000000) ERROR_DIMENSIONS_INVALID)
    (asserts! (> (len synopsis) u0) ERROR_IMPROPER_DESCRIPTOR)
    (asserts! (< (len synopsis) u257) ERROR_IMPROPER_DESCRIPTOR)
    (asserts! (validate-classification-set tags) ERROR_IMPROPER_DESCRIPTOR)

    ;; Record the scholarly contribution
    (map-insert scholarly-entries
      { entry-id: entry-id }
      {
        work-descriptor: descriptor,
        scholar-identity: tx-sender,
        work-magnitude: magnitude,
        submission-block: block-height,
        work-synopsis: synopsis,
        classification-tags: tags
      }
    )

    ;; Establish creator access privileges
    (map-insert viewing-permissions
      { entry-id: entry-id, viewer: tx-sender }
      { permission-granted: true }
    )

    ;; Update system counter
    (var-set entry-counter entry-id)
    (ok entry-id)
  )
)

;; Alternative submission pathway with semantic variations
(define-public (catalog-intellectual-work (descriptor (string-ascii 80)) (magnitude uint) (synopsis (string-ascii 256)) (tags (list 8 (string-ascii 40))))
  (let
    (
      (entry-id (+ (var-get entry-counter) u1))
    )
    ;; Comprehensive input validation
    (asserts! (> (len descriptor) u0) ERROR_IMPROPER_DESCRIPTOR)
    (asserts! (< (len descriptor) u81) ERROR_IMPROPER_DESCRIPTOR)
    (asserts! (> magnitude u0) ERROR_DIMENSIONS_INVALID)
    (asserts! (< magnitude u2000000000) ERROR_DIMENSIONS_INVALID)
    (asserts! (> (len synopsis) u0) ERROR_IMPROPER_DESCRIPTOR)
    (asserts! (< (len synopsis) u257) ERROR_IMPROPER_DESCRIPTOR)
    (asserts! (validate-classification-set tags) ERROR_IMPROPER_DESCRIPTOR)

    ;; Persist scholarly metadata in repository
    (map-insert scholarly-entries
      { entry-id: entry-id }
      {
        work-descriptor: descriptor,
        scholar-identity: tx-sender,
        work-magnitude: magnitude,
        submission-block: block-height,
        work-synopsis: synopsis,
        classification-tags: tags
      }
    )

    ;; Configure default access control settings
    (map-insert viewing-permissions
      { entry-id: entry-id, viewer: tx-sender }
      { permission-granted: true }
    )

    ;; Increment global identifier sequence
    (var-set entry-counter entry-id)
    (ok entry-id)
  )
)

;; =============================================================================
;; |                            ENTRY MANAGEMENT                                |
;; =============================================================================

;; Updates an existing scholarly work's metadata
(define-public (revise-scholarly-entry (entry-id uint) (updated-descriptor (string-ascii 80)) (updated-magnitude uint) (updated-synopsis (string-ascii 256)) (updated-tags (list 8 (string-ascii 40))))
  (let
    (
      (entry-data (unwrap! (map-get? scholarly-entries { entry-id: entry-id }) ERROR_ENTRY_NONEXISTENT))
    )
    ;; Verify entry existence and ownership
    (asserts! (entry-exists entry-id) ERROR_ENTRY_NONEXISTENT)
    (asserts! (is-eq (get scholar-identity entry-data) tx-sender) ERROR_PERMISSION_INSUFFICIENT)

    ;; Validate updated descriptor
    (asserts! (> (len updated-descriptor) u0) ERROR_IMPROPER_DESCRIPTOR)
    (asserts! (< (len updated-descriptor) u81) ERROR_IMPROPER_DESCRIPTOR)

    ;; Validate updated magnitude
    (asserts! (> updated-magnitude u0) ERROR_DIMENSIONS_INVALID)
    (asserts! (< updated-magnitude u2000000000) ERROR_DIMENSIONS_INVALID)

    ;; Validate updated synopsis
    (asserts! (> (len updated-synopsis) u0) ERROR_IMPROPER_DESCRIPTOR)
    (asserts! (< (len updated-synopsis) u257) ERROR_IMPROPER_DESCRIPTOR)

    ;; Validate updated tags
    (asserts! (validate-classification-set updated-tags) ERROR_IMPROPER_DESCRIPTOR)

    ;; Update entry with revised information
    (map-set scholarly-entries
      { entry-id: entry-id }
      (merge entry-data { 
        work-descriptor: updated-descriptor, 
        work-magnitude: updated-magnitude, 
        work-synopsis: updated-synopsis, 
        classification-tags: updated-tags 
      })
    )
    (ok true)
  )
)

;; Permanently removes a scholarly work from the repository
(define-public (withdraw-scholarly-work (entry-id uint))
  (let
    (
      (entry-data (unwrap! (map-get? scholarly-entries { entry-id: entry-id }) ERROR_ENTRY_NONEXISTENT))
    )
    ;; Verify entry exists
    (asserts! (entry-exists entry-id) ERROR_ENTRY_NONEXISTENT)
    ;; Ensure only the owner can withdraw the work
    (asserts! (is-eq (get scholar-identity entry-data) tx-sender) ERROR_PERMISSION_INSUFFICIENT)

    ;; Remove the entry from repository
    (map-delete scholarly-entries { entry-id: entry-id })
    (ok true)
  )
)

;; =============================================================================
;; |                             QUERY INTERFACES                               |
;; =============================================================================

;; Retrieves presentation-ready formatting of scholarly work
(define-public (generate-work-visualization (entry-id uint))
  (let
    (
      (entry-data (unwrap! (map-get? scholarly-entries { entry-id: entry-id }) ERROR_ENTRY_NONEXISTENT))
    )
    ;; Format structured display information
    (ok {
      page-title: "Scholarly Work Details",
      work-descriptor: (get work-descriptor entry-data),
      scholar-identity: (get scholar-identity entry-data),
      work-synopsis: (get work-synopsis entry-data),
      classification-tags: (get classification-tags entry-data)
    })
  )
)

;; Retrieves core metadata with optimized efficiency
(define-public (retrieve-core-metadata (entry-id uint))
  (let
    (
      (entry-data (unwrap! (map-get? scholarly-entries { entry-id: entry-id }) ERROR_ENTRY_NONEXISTENT))
    )
    ;; Return optimized core data structure
    (ok {
      work-descriptor: (get work-descriptor entry-data),
      scholar-identity: (get scholar-identity entry-data),
      work-magnitude: (get work-magnitude entry-data)
    })
  )
)

;; Produces comprehensive information display for scholarly work
(define-public (compile-detailed-record (entry-id uint))
  (let
    (
      (entry-data (unwrap! (map-get? scholarly-entries { entry-id: entry-id }) ERROR_ENTRY_NONEXISTENT))
    )
    ;; Format complete record for presentation layer
    (ok {
      descriptor: (get work-descriptor entry-data),
      author: (get scholar-identity entry-data),
      magnitude: (get work-magnitude entry-data),
      synopsis: (get work-synopsis entry-data),
      tags: (get classification-tags entry-data)
    })
  )
)

;; Optimized minimal data retrieval for high performance contexts
(define-public (fetch-basic-identification (entry-id uint))
  (let
    (
      (entry-data (unwrap! (map-get? scholarly-entries { entry-id: entry-id }) ERROR_ENTRY_NONEXISTENT))
    )
    ;; Return minimal identification data for maximum efficiency
    (ok {
      work-descriptor: (get work-descriptor entry-data),
      scholar-identity: (get scholar-identity entry-data)
    })
  )
)

;; Extracts only the synopsis component for lightweight access
(define-public (extract-work-synopsis (entry-id uint))
  (let
    (
      (entry-data (unwrap! (map-get? scholarly-entries { entry-id: entry-id }) ERROR_ENTRY_NONEXISTENT))
    )
    (ok (get work-synopsis entry-data))
  )
)

;; =============================================================================
;; |                        VALIDATION FRAMEWORK                                |
;; =============================================================================

;; Validates input parameters without creating repository entry
(define-public (validate-submission-criteria (descriptor (string-ascii 80)) (magnitude uint) (synopsis (string-ascii 256)) (tags (list 8 (string-ascii 40))))
  (begin
    ;; Validate descriptor constraints
    (asserts! (> (len descriptor) u0) ERROR_IMPROPER_DESCRIPTOR)
    (asserts! (< (len descriptor) u81) ERROR_IMPROPER_DESCRIPTOR)

    ;; Validate magnitude constraints
    (asserts! (> magnitude u0) ERROR_DIMENSIONS_INVALID)
    (asserts! (< magnitude u2000000000) ERROR_DIMENSIONS_INVALID)

    ;; Validate synopsis constraints
    (asserts! (> (len synopsis) u0) ERROR_IMPROPER_DESCRIPTOR)
    (asserts! (< (len synopsis) u257) ERROR_IMPROPER_DESCRIPTOR)

    ;; Validate classification tags constraints
    (asserts! (validate-classification-set tags) ERROR_IMPROPER_DESCRIPTOR)

    (ok true)
  )
)

