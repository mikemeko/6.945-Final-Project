(define parser (make-tree start-tag? end-tag? self-terminating-tag? create-xml-node))
(define root (parser (xml-tokenize (read-file "example.xml"))))

(pp (stringify rss-example))

(if RUN-TESTS (begin
 ;RSS EXAMPLE TEST Example Test
 (assert (equal? (stringify rss-example) 
  "<xml version=\"1.0\" encoding=\"ISO-8859-1\"></xml><rss version=3.0><channel><title>W3Schools Home Page</title></channel></rss>") 
  "Fails RSS Example")

; END-TO-END TEST
  (pp (stringify root))
  (write-tree root "test-output.txt")

))
