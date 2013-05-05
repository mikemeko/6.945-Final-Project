;; XML Tokenizer

;;; Predicate for XML tag tokens
(define (is-tag-token? text)
  (and (starts-with text "<")
       (ends-with text ">")
       (is-text-token? (substring text 1 (- (string-length text) 1)))))

;;; Predicate for XML text tokens
(define (is-text-token? text)
  (and (not (string-find-next-char text #\<))
       (not (string-find-next-char text #\>))))

;;; Predicate for all XML tokens
(define (is-xml-token? text)
  (or (is-tag-token? text) (is-text-token? text)))

;;; Tokenizer for xml text
(define xml-tokenize (tokenizer is-xml-token?))

;; Examples
#|
(xml-tokenize "")
> ()
(xml-tokenize "x")
> ("x")
(xml-tokenize "<p>")
> ("<p>")
(xml-tokenize "<p>x</p>")
> ("<p>" "x" "</p>")
(xml-tokenize "<p")
> error: no token starting at index 0
(xml-tokenize "p>")
> error: no token starting at index 1
(xml-tokenize "<p>>")
> error: no token starting at index 3
(xml-tokenize "<<p>")
> error: no token starting at index 0
|#

;;; tokenizes the given |text| into tokens, where a token is either a block of
;;;     text, called TAG, starting with a "<" and ending with a ">" or a block
;;;     of text that sits between two TAGs.
;;; This code is currently is NOT used anywhere
(define (deprecated-xml-tokenize text)
  (assert (string? text) "text has to be a string")
  (let ((tokens '()) (n (string-length text)))
    (define (append-token token)
      (set! tokens (append tokens (list token))))
    (let loop-i ((i 0))
      (if (= i n)
          tokens
          (let* ((char-i (string-ref text i))
                 (tag-start-i? (eq? char-i #\<))
                 (tag-end-i? (eq? char-i #\>)))
            (if tag-end-i?
              (error "malformed text")
              (let loop-j ((j (+ i 1)))
                (if (= j n)
                  (if tag-start-i?
                      (error "malformed text")
                      (begin (append-token (substring text i j)) tokens))
                  (let* ((char-j (string-ref text j))
                         (tag-start-j? (eq? char-j #\<))
                         (tag-end-j? (eq? char-j #\>)))
                    (cond ((and (not tag-start-i?) tag-end-j?)
                           (error "malformed text"))
                          ((and tag-start-i? tag-end-j?)
                           (begin (append-token (substring text i (+ j 1)))
                                  (loop-i (+ j 1))))
                          ((and (not tag-start-i?) tag-start-j?)
                           (begin (append-token (substring text i j))
                                  (loop-i j)))
                          (else (loop-j (+ j 1)))))))))))))

;; Examples
#|
(deprecated-xml-tokenize "")
> ()
(deprecated-xml-tokenize "x")
> ("x")
(deprecated-xml-tokenize "<p>")
> ("<p>") 
(deprecated-xml-tokenize "<p>x</p>")
> ("<p>" "x" "</p>")
(deprecated-xml-tokenize "<p")
> error: malformed text
(deprecated-xml-tokenize "p>")
> error: malformed text
(deprecated-xml-tokenize "<p>>")
> error: malformed text
|#

