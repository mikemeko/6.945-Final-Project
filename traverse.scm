; ****************************************************
; This file is for methods dependent on the structure of the tree.
; Structure-independent methods dealing only with nodes are in node.scm

; NOTES ON TREE REPRESENTATION:

; Tree segments are represented as: (cons parent-node (list child1 child2)
; where child where child1 and child2 are cons pairs

; There are two types of nodes: tags and non-tags.
; Tags have data that are a lists of cons pairs
; corresponding to attributes
; Non-tags have corresponding text data in the same
; form (e.g. (cons "text" "Hello World!"))

; USER-MODEL

; Note: I've been trying to figure out how to best model 
; the user's interaction so that its easy for the user to conceptualize.  
; For now, I'm going to keep it as simple as possible:

; The user interfaces exclusively with tree segments.  
; That is, any user-facing accessor will 
; take in a single tree segment and return either
; A) another single tree segment
; B) very specific data (tag name, specific attribute data,
; list of attributes it has, etc) as a string, 
; list of strings, or number
;
; This is a less complex UI model than what beautiful soup uses
; (which has Tag, Attribute, etc objects as part of the UI)

; ****************************************************

; CURRENTLY MODELED AS PRIVATE (EVEN THOUGH NOT)

; Takes in a tree segment and tag
; Returns list of tree segments whose roots
; are children with the given tag
; Returns '() if none exist
(define (filter-descendents-by-tag segment tag)
    ; Takes in a list of tree segments and a tag
    ; Returns either 
    ; a) list of tree segments with roots that
    ; match that tag if one exists
    ; b) '() if none exist
    (define (filter-list-by-tag segments tag)
      (define (compare-tag x)
        (equal? (get-tag (car x)) tag)
      )
      (list-transform-positive segments compare-tag))
    (filter-list-by-tag (cdr segment) tag))

; checks tree segment representation
(define (checkRep segment)
  ; currently quick, very non-comprehensive check of input format
  (assert (list? segment) "input should be a tree segment")
  (define (list-or-node? x)
    (assert (or (node? x) (list? x)) "input should be a tree segment") 
  )
  (for-each list-or-node? segment)
)

; CURRENTLY MODELED AS PUBLIC

(define (tag segment)
  (get-tag (car segment))
)

(define (change-tag segment new-tag)
  (set-tag (car segment) new-tag)
)

; returns list of attributes (not values)
(define (attributes segment)

  ; TODO(pauL): add check that makes sure its data is attributes

  (map (lambda (x) (car x)) (get-data (car segment)))
)

(define (get-attribute segment attribute)
  '*nothing*
)


(define (change-attribute segment attribute new-value)
  '*nothing*
)


; Takes in a tree segment (list of lists and nodes)
; returns list of tree sections with roots that 
; are children of given tree segment root
; Returns '() if none exist

(define (count segment)
  (length (cdr segment))
)

(define (walk segment pos)
  (checkRep segment)
  (list-ref (cdr segment) pos)
)

; Takes in tree segment and tag
; Returns how many child segments that has a root
; of the given tag
(define (count-tag segment tag)
  (let ((filtered-descendents (filter-descendents-by-tag segment tag)))
    (length filtered-descendents)))

; Takes in tree segment, tag, and position
; Returns the pos'th child segment that has a root
; of the given tag
; (i.e. (get "head" html 0) (get "channel" rss 0)
(define (walk-by-tag segment tag pos)
  (let ((filtered-descendents (filter-descendents-by-tag segment tag)))
    (assert (> (length filtered-descendents) pos) "out-of-range")
        (list-ref filtered-descendents pos)))
