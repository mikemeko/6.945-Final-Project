; ****************************************************
; NOTES ON TREE REPRESENTATION:

; Tree segments are represented as: (cons parent-node (list child1 child2)
; where child where child1 and child2 are cons pairs

; There are two types of nodes: tags and non-tags.
; Tags have data that are a lists of cons pairs
; corresponding to attributes
; Non-tags have corresponding text data in the same
; form (e.g. (cons text "Hello World!"))
; TODO(mikemeko, pwoods): text -> 'text ?

; ****************************************************


; METHODS DEPENDENT STRUCTURE OF TREE

; Takes in a given tree segment (list of lists and nodes)
; returns list of tree sections with roots that 
; are children of given tree section root
; Returns '() if none exist

(define (descendents tree-segment)
  ; quick, non-comprehensive check of input format
  (assert (list? tree-segment) "input should be a tree segment")
  (define (list-or-node? x)
    (assert (or (node? x) (list? x)) "input should be a tree segment") 
  )
  (for-each list-or-node? tree-segment)

  ; return children of tree-segment
  (cdr tree-segment)
)

; Takes in a given a given tree segment (list of lists and nodes)
; and tag
; Returns either 
; a) list of tree sections with roots that
; match that tag if one exists
; b) '() if none exist
(define (filter-by-tag tree-segment tag)

  ; DOESN'T YET WORK PROPERLY

  (list-transform-positive tree-segment
    (lambda(x) (eq? (get-tag (car x)) tag)))
)

