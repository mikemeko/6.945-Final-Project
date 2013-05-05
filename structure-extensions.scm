; ************************
; MODIFYING TREE STRUCTURE
; ************************

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

