(pp "Testing I/O")
(define root (make-tree (tokenize (read-file "example.xml"))))
;(pp (stringify root))
(pp (stringify rss-example))
