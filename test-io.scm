
(if RUN-TESTS (begin
; RSS EXAMPLE TEST Example Test
(assert (equal? (stringify rss-example) 
  "<xml version=\"1.0\" encoding=\"ISO-8859-1\"></xml><rss version=3.0><channel><title>W3Schools Home Page</title></channel></rss>") 
  "Fails RSS Example")

; END-TO-END TEST
  ;(define root (make-tree (tokenize (read-file "example.xml"))))
  ;(pp (stringify root))
  ;(write-tree root "test-output.txt")

))
