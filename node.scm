(define-record-type node
    (make-node tag data)
    node?
    (tag get-tag set-tag)
    (data get-data set-data))


; Takes in node
; Returns #t if node represents a tag and #f
; if node represents loose data
(define (any-tag? node)
  (not (eq? (get-tag node) 'non-tag))
)


