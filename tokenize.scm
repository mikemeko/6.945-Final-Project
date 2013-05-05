;; XML Tokenizer

;;; Strips off the auxillary strings of a tag (including the outer brackets/slashes
;;; Assumes the tag is of valid XML syntax
(define (get-element tag)
    (let ((left (if (string-prefix? "</" tag) 2 1))
          (right (if (string-suffix? "/>" tag) (- (string-length tag) 2) (- (string-length tag) 1))))
        (substring tag left right)))

;;; tokenizes the given |text| into tokens, where a token is either a block of
;;;     text, called TAG, starting with a "<" and ending with a ">" or a block
;;;     of text that sits between two TAGs.
;;; This code is currently is NOT used anywhere
(define (deprecated-tokenize text)
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
(tokenize "p>")
> error: malformed text
(tokenize "<p>>")
> error: malformed text
|#

;;; Splits |text| by the given |delimiter| and returns a list containing the parts.
;;; This code is currently is NOT used anywhere
(define (deprecated-split text delimiter)
  (assert (string? text) "text has to be a string")
  (assert (char? delimiter) "delimiter has to be a char")
  (let ((parts '()) (n (string-length text)))
    (define (append-part part)
      (if (> (string-length part) 0)
          (set! parts (append parts (list part)))))
    (let loop-i ((i 0))
      (if (= i n)
          parts
          (let* ((char-i (string-ref text i))
                 (delimiter-i? (eq? char-i delimiter)))
            (if delimiter-i?
                (loop-i (+ i 1))
                (let loop-j ((j (+ i 1)))
                  (if (= j n)
                      (begin (append-part (substring text i j)) parts)
                      (let* ((char-j (string-ref text j))
                             (delimiter-j? (eq? char-j delimiter)))
                        (if delimiter-j?
                            (begin (append-part (substring text i j)) (loop-i (+ j 1)))
                            (loop-j (+ j 1))))))))))))

;; Examples
#|
(split "a b c" #\ )
> ("a" "b" "c")
(split "a       b       c" #\ )
> ("a" "b" "c")
(split "<p a= 'hi'/>" #\ )
> ("<p" "a=" "'hi'/>")
|#

;;; Returns a function that takes in a piece of text and returns a list of the tokens
;;;    in the text, where the meaning of a token is given by the predicate |is-token?|
(define (tokenizer is-token?)
  (lambda (text)
    (let ((tokens '()) (n (string-length text)))
      (define (append-token token)
        (set! tokens (append tokens (list token))))
      (let loop-i ((i 0))
        (if (= i n)
            tokens
            ;; store current ending index; largest found token; index to restart from
            (let loop-j ((j (+ i 1)) (found-token 'none) (restart-index -1))
              (if (> j n)
                  (if (not (eq? found-token 'none))
                      (begin (append-token found-token) tokens)
                      (error "no token starting at index" i))
                  (let ((substr (substring text i j)))
                    (if (is-token? substr)
                        (loop-j (+ j 1) substr j)
                        (if (not (eq? found-token 'none))
                            (begin (append-token found-token)
                                   (loop-i restart-index))
                            (loop-j (+ j 1) found-token restart-index)))))))))))

;;; Predicate for XML tag tokens
(define (is-tag-token? text)
  (and (starts-with text "<") (ends-with text ">")))

;;; Predicate for XML text tokens
(define (is-text-token? text)
  (and (not (string-find-next-char text #\<))
       (not (string-find-next-char text #\>))))

;;; Predicate for all XML tokens
(define (is-xml-token? text)
  (or (is-tag-token? text) (is-text-token? text)))

;; Examples
#|
(define t (tokenizer is-xml-token?))
(t "")
> ()
(t "x")
> ("x")
(t "<p>")
> ("<p>")
(t "<p>x</p>")
> ("<p>" "x" "</p>")

(define (all-the-same? text)
  (let ((n (string-length text)))
    (let loop ((i 0))
      (if (= i n)
          #t
          (if (eq? (string-ref text 0) (string-ref text i))
              (loop (+ i 1))
              #f)))))
(define t (tokenizer all-the-same?))
(t "abc")
> ("a" "b" "c")
(t "aabbcc")
> ("aa" "bb" "cc")
|#

