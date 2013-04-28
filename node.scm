(define-record-type node
    (make-node type list-of-attrs)
    node?
    (type get-type)
    (list-of-attrs get-attrs))
