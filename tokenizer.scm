;; XML Tokenizer

;;; Strips off the auxillary strings of a tag (including the outer brackets/slashes
;;; Assumes the tag is of valid XML syntax
;;; TODO(mikemeko): move elsewhere
(define (get-element tag)
    (let ((left (if (string-prefix? "</" tag) 2 1))
          (right (if (string-suffix? "/>" tag) (- (string-length tag) 2) (- (string-length tag) 1))))
        (substring tag left right)))

;;; Splits |text| by the given |delimiter| and returns a list containing the parts.
;;; This code is currently is NOT used anywhere
;;; TODO(mikemeko): delete?
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

;; Examples
#|
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

