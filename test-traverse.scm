#|

The following test corresponds to the below example 
translated into tree form:

<?xml version="1.0" encoding="ISO-8859-1" ?>
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
(define xml (new-tag "xml" (list (cons "version" "\"1.0\"") (cons "encoding" "\"ISO-8859-1\"")) '()))
(define rss-example (new-root (list xml rss)))

;(define xml (new-tag "xml" (list (cons "version" "\"1.0\"") (cons "encoding" "\"ISO-8859-1\"")) '()))
;(define rss-example (new-root (list xml)))

;(add-child rss-example xml)

(if RUN-TESTS (begin
  (pp "")
  (pp "Testing Tree Traversal")

  #|
  (pp rss-example)

  ;(pp (delete rss-example 0))
  ;(pp (add rss-example test))
  ;(pp (add rss-example (cons rss (list (cons test '())))))
  ;(pp (delete rss-example 2))
  ;(pp (delete rss-example 2))

  (pp (count rss-example))
  (pp (walk rss-example 0))
  (pp (walk rss-example 1))
  (pp (count-tag rss-example "rss"))
  (pp (walk-by-tag rss-example "rss" 0))


  ;(pp (attributes (walk-by-tag rss-example "rss" 0)))
  ;(pp (get-attribute (walk-by-tag rss-example "rss" 0) "version"))
  ;(pp (remove-attribute (walk-by-tag rss-example "rss" 0) "version"))
  ;(pp (attributes (walk-by-tag rss-example "rss" 0)))
  ;(pp (set-attribute (walk-by-tag rss-example "rss" 0) "version" "3.0"))
  ;(pp (attributes (walk-by-tag rss-example "rss" 0)))
  ;(pp (get-attribute (walk-by-tag rss-example "rss" 0) "version"))

  (pp (tag (walk rss-example 0)))
  ;(modify-tag (walk-by-tag rss-example "rss" 0) "hi")
  (pp (tag (walk rss-example 0)))
  (pp (attributes (walk rss-example 0)))
  |#

))
