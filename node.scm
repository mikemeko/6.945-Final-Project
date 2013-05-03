(define-record-type node
    (make-node tag data)
    node?
    (tag get-tag set-tag)
    (data get-data set-data))

