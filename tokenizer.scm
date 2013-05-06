;; Tokenizer

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

