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

(define-record-type relationship
  (make-relationship parent children)
  relationship?
  (parent get-parent set-parent)
  (children get-children set-childrens))

; Takes in a tree segment and pos
; Returns list of child segments
(define (children segment)
  (get-children segment))

; Takes in a tree segment and list of tree segments
; Returns segment with children set to list 
(define (set-children segment new-children)
  (set-childrens segment new-children))

; Takes in node and list of children segments
; Returns segment representation
(define (new-segment current-node children)
  (make-relationship current-node children))

; Takes in a tree segment
; Returns corresponding node
(define (current segment)
  (get-parent segment))
