; ***********************************************************
; Abstracts away representation of relationships between nodes

; This file defines the representation of tree
; structure. These procedures deal only with the structure
; of tree (parent-child relationship between nodes) and not 
; with nodes themselves.  This file is XML-independent. 

; Procedures in all other files
; should be independent of the tree representation and
; call these procedures to access anything dependent on
; the tree representation.
;
; NOTES ON TREE REPRESENTATION:
;
; Tree segments are represented as: (cons parent-node (list child1 child2)
; where child where child1 and child2 are cons pairs (relationships aka
; segments)

; ****************************************************

; Takes in a tree segment and pos
; Returns list of child segments


(define (children segment)
  (cdr segment))

(define (set-children segment new-children)
  (set-cdr! segment new-children)
)

(define (new-relationship current-node children)
  (cons current-node children)
)

; Takes in a tree segment and pos
; Returns list of child segments
(define (current segment)
  (car segment))


