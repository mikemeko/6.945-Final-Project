;;; A record type for a basic XML element. The tag
;;; is the name of the element (such as p for <p>),
;;; and data is the list of attributes (for example,
;;; data would be ("a" . "hello") for <p a="hello">)
;;; Text nodes have the special tag "text", and comment
;;; nodes have the special tag "comment".
(define-record-type node
    (make-node tag data)
    node?
    (tag get-tag set-tag)
    (data get-data set-data))

