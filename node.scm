(define-record-type node
    (make-node type list-of-attrs)
    node?
    (type get-type set-type)
    (list-of-attrs get-attrs set-attrs))
