;; XML Tokenizer

;;; tokenizes the given |text| into tokens, where a token is either a block of
;;;     text, called TAG, starting with a "<" and ending with a ">" or a block
;;;     of text that sits between two TAGs.
(define (tokenize text)
  (assert (string? text) "text has to be a string")
  (let ((tokens '()) (n (string-length text)))
    (define (append-token token)
      (set! tokens (append tokens (list token))))
    (let loop-i ((i 0))
      (if (= i n)
          tokens
          (let* ((char-i (string-ref text i))
                 (tag-start-i (eq? char-i #\<)))
            (let loop-j ((j (+ i 1)))
              (if (= j n)
                (if tag-start-i
                    (error "malformed text")
                    (begin (append-token (substring text i j)) tokens))
                (let* ((char-j (string-ref text j))
                       (tag-start-j (eq? char-j #\<))
                       (tag-end-j (eq? char-j #\>)))
                  (cond ((and tag-start-i tag-end-j)
                         (begin (append-token (substring text i (+ j 1)))
                                (loop-i (+ j 1))))
                        ((and (not tag-start-i) tag-start-j)
                         (begin (append-token (substring text i j))
                                (loop-i j)))
                        (else (loop-j (+ j 1))))))))))))

;; Examples
#|
(tokenize "")
> ()
(tokenize "x")
> ("x")
(tokenize "<p>")
> ("<p>") 
(tokenize "<p>x</p>")
> ("<p>" "x" "</p>")
(tokenize "<p")
> error: malformed text
|#

