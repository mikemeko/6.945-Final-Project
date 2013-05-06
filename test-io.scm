#|

The following test corresponds to the below example 
translated into tree form:

<rss version="2.0">

<channel>
<title>W3Schools Home Page</title>
</channel>

</rss>
|#

(define text (new-text "W3Schools Home Page"))
(define title (new-tag "title" '() (list text)))
(define channel (new-tag "channel" '() (list title)))
(define rss (new-tag "rss" (list (cons "version" "\"2.0\"")) (list channel)))
(define rss-example (new-root (list rss)))
(define root (xml-parse (xml-tokenize (read-file "example.xml"))))

(pp "stingify")
(pp (stringify rss-example))

(if RUN-TESTS (begin
 
 ;RSS EXAMPLE TEST Example Test
 (assert (equal? (stringify rss-example) 
  "<rss version=\"2.0\"><channel><title>W3Schools Home Page</title></channel></rss>") 
  "Fails RSS Example")

  ; END-TO-END TEST
  (write-tree root "test-output.txt")
))
