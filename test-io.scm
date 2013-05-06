(define root (xml-parse (xml-tokenize (read-file "example.xml"))))

(if RUN-TESTS (begin
 
 ;RSS EXAMPLE TEST Example Test
 (assert (equal? (stringify rss-example) 
  "<xml version=\"1.0\" encoding=\"ISO-8859-1\"></xml><rss version=\"2.0\"><channel><title>W3Schools Home Page</title></channel></rss>") 
  "Fails RSS Example")

  ; END-TO-END TEST
  (write-tree root "test-output.txt")
))
