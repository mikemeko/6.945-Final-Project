; ****************************************************
; NOTES ON TREE REPRESENTATION:

; Tree segments are represented as: (cons parent-node (list child1 child2)
; where child where child1 and child2 are cons pairs

; There are two types of nodes: tags and non-tags.
; Tags have data that are a lists of cons pairs
; corresponding to attributes
; Non-tags have corresponding text data in the same
; form (e.g. (cons "text" "Hello World!"))

; ****************************************************


; METHODS DEPENDENT STRUCTURE OF TREE

; Takes in a tree segment (list of lists and nodes)
; returns list of tree sections with roots that 
; are children of given tree section root
; Returns '() if none exist

(define (checkRep segment)
  #t 
)

(define (descendents segment)
  ; quick, non-comprehensive check of input format
  (assert (list? segment) "input should be a tree segment")
  (define (list-or-node? x)
    (assert (or (node? x) (list? x)) "input should be a tree segment") 
  )
  (for-each list-or-node? segment)

  ; return children of segment
  (cdr segment)
)

; Takes in a list of tree segments and a tag
; Returns either 
; a) list of tree segments with roots that
; match that tag if one exists
; b) '() if none exist
(define (filter-list-by-tag segments tag)

  (define (compare-tag x)
    (equal? (get-tag (car x)) tag)
  )
  (list-transform-positive segments compare-tag)
)

; Takes in a tree segment and tag
; Returns list of tree segments whose roots
; are children with the given tag
; Returns '() if none exist
(define (filter-descendents-by-tag segment tag)
  (filter-list-by-tag (descendents segment) tag) 
)

