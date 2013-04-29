
(define debug-output #t)

#|

This test corresponds to the below example 
translated into tree form:

<?xml version="1.0" encoding="ISO-8859-1" ?>
<rss version="2.0">

<channel>
<title>W3Schools Home Page</title>
</channel>

</rss>
|#


(define root (make-node '*nothing* '()))
(define xml (make-node "xml" (list (cons "version" "1.0") (cons "encoding" "ISO-8859-1"))))  ; temp
(define rss (make-node "RSS" (list (cons "version" "2.0"))))
(define channel (make-node "channel" '()))
(define title (make-node "title" '()))
(define text (make-node 'non-tag (list (cons "text" "W3Schools Home Page"))))

(define rss-example (cons root (list (cons xml '()) (cons rss (list (cons channel (list (cons title (list (cons text '()))))))))))

(if debug-output (begin 
  (pp "")
  (pp "Testing Tree Traversal")
  (pp (descendents rss-example))
  (pp (filter-by-tag (descendents rss-example) "xml"))
  
  ))
