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
