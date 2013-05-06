;;; Returns true if the argument is a the beginning of a tag, a la <p> or <div>
(define (xml-start-tag? str)
    (and (string-prefix? "<" str)
         (not (string-suffix? "/>" str))
         (not (eq? #\/ (string-ref str 1)))))

;;; Returns true if the argument is a closing tag, a la </p> or </div>
(define (xml-end-tag? str)
    (string-prefix? "</" str))

;;; Returns true if the argument is a self-terminating tag, a la <a
;;; href="http://groups.csail.mit.edu/mac/users/gjs/6.945/"/>
(define (xml-self-terminating-tag? str)
    (and (string-prefix? "<" str)
         (string-suffix? "/>" str)))

;;; Returns true if the argument is a comment tag, a la <!-- comment -->
(define (xml-comment? str)
  (and (string-prefix? "<!--" str)
       (string-suffix? "-->" str)))

;;; Strips off the auxillary strings of a tag (including the outer brackets/slashes
;;; Assumes the tag is of valid XML syntax
(define (get-element tag)
   (define (strip-prefix tag)
     (cond ((string-prefix? "<!--" tag) (string-tail tag 4))
           ((string-prefix? "</" tag) (string-tail tag 2))
           ((string-prefix? "<" tag) (string-tail tag 1))))
    (define (strip-suffix tag)
      (let ((n (string-length tag)))
        (cond ((string-suffix? "-->" tag) (string-head tag (- n 3)))
              ((string-suffix? "/>" tag) (string-head tag (- n 2)))
              ((string-suffix? ">" tag) (string-head tag (- n 1))))))
    (strip-prefix (strip-suffix tag)))

;;; Return a pair of the following form (type (key-1 . attr-1) (key-2 . attr-2) ...)
;;; Assumes the tag argument is a valid XML tag
(define (get-type-and-attributes tag)
  (let* ((element (string-append (get-element tag) " "))
         (n (string-length element))
         (type)
         (attributes '())
         (current-attribute-key))
    (define (eat-white-space i)
      (let loop ((j i))
        (if (and (< j n) (eq? (string-ref element j) #\ ))
            (loop (+ j 1))
            j)))
    (define (get-type i)
      (let ((next-space-index (substring-find-next-char-ci element i n #\ )))
        (set! type (substring element i next-space-index))
        (let ((j (eat-white-space (+ next-space-index 1))))
          (get-attribute-key j))))
    (define (get-attribute-key i)
      (if (not (= i n))
          (let ((next-equal-index (substring-find-next-char-ci element i n #\=)))
            (set! current-attribute-key (substring element i next-equal-index))
            (let ((j (eat-white-space (+ next-equal-index 1))))
              (if (eq? (string-ref element j) #\")
                  (get-attribute-value j #\")
                  (get-attribute-value j #\ ))))))
    (define (get-attribute-value i end-char)
      (let ((end-char-index (substring-find-next-char-ci  element (+ i 1) n end-char)))
        (set! attributes (append attributes 
                                (list (cons current-attribute-key
                                            (substring element i (+ end-char-index 1))))))
        (let ((j (eat-white-space (+ end-char-index 1))))
          (get-attribute-key j))))
    (define (start)
      (let ((j (eat-white-space 0)))
        (get-type j)))
    (start)
    (cons type attributes)))

;;; Takes in a token and returns an XML node
(define (create-xml-node token)
  (cond ((xml-comment? token)
         (make-node 'non-tag (list (cons "comment" (get-element token)))))
        ((and (not (xml-self-terminating-tag? token))
              (not (xml-start-tag? token))
              (not (xml-end-tag? token)))
         (make-node 'non-tag (list (cons "text" token))))
        (else
         (let ((parameters (get-type-and-attributes token)))
           (make-node (car parameters) (cdr parameters))))))

;;; Parser for xml tokens
(define xml-parse (make-tree xml-start-tag? xml-end-tag? create-xml-node))

;; Examples
#|
(xml-parse '("<p a=1>" "hello" "</p>"))
>(#[node 24] (#[node 25] #[node 26])) 
(xml-parse (caadr (make-tree '("<p a=1>" "hello" "</p>")))) 
> "p"
|#

