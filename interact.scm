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
; ****************************************************

(load STRUCTURE-FILE)

; Takes in two tree segments
; Adds one as child of other
(define (add-child segment child)
  (set-children segment (append (children segment) (list child)))
  segment
)

; Takes in tree segment and integer
; Returns tree segment with pos'th child deleted
(define (delete-child segment pos)
  (assert (< pos (count segment)) "out of range")
  (set-children segment (delete (list-ref (children segment) pos) (children segment)))
  segment
)

