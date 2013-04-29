;;; Returns true if the argument is a the beginning of a tag, a la <p> or <div>
(define (start-tag? str)
    (and (string-prefix? "<" str)
         (not (string-suffix? "/>" str))
         (not (eq? #\/ (string-ref str 1)))))

;;; Returns true if the argument is a closing tag, a la </p> or </div>
(define (end-tag? str)
    (string-prefix? "</" str))

;;; Returns true if the argument is a self-terminating tag, a la <a
;;; href="http://groups.csail.mit.edu/mac/users/gjs/6.945/"/>
(define (self-terminating-tag? str)
    (and (string-prefix? "<" str)
         (string-suffix? "/>" str)))

;;; Return a pair of the following form (type (key-1 . attr-1) (key-2 . attr-2) ...)
;;; Assumes the tag argument is a valid XML tag
(define (get-type-and-attributes tag)
  (let* ((element (string-append (get-element tag) " "))
         (n (string-length element))
         (type)
         (attributes '())
         (current-attribute-key))
    ; (pp element)
    (define (eat-white-space i)
      ; (pp 'eat-white-space) (pp i)
      (let loop ((j i))
        (if (and (< j n) (eq? (string-ref element j) #\ ))
            (loop (+ j 1))
            j)))
    (define (get-type i)
      ; (pp 'get-type) (pp i)
      (let ((next-space-index (substring-find-next-char-ci element i n #\ )))
        (set! type (substring element i next-space-index))
        (let ((j (eat-white-space (+ next-space-index 1))))
          (get-attribute-key j))))
    (define (get-attribute-key i)
      ; (pp 'get-attribute-key) (pp i)
      (if (not (= i n))
          (let ((next-equal-index (substring-find-next-char-ci element i n #\=)))
            (set! current-attribute-key (substring element i next-equal-index))
            (let ((j (eat-white-space (+ next-equal-index 1))))
              (if (eq? (string-ref element j) #\")
                  (get-attribute-value j #\")
                  (get-attribute-value j #\ ))))))
    (define (get-attribute-value i end-char)
      ; (pp 'get-attribute-value-quote) (pp i)
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
  
;;; Takes in a tag token and returns a node object
;;; Assumes that token is a a start tag or a self-terminating tag
#|
(define (get-node token)
    (define (get-type str)
        (substring str 1 (string-find-next-char str #\ )))
    (define (get-attributes str attributes)
        (let ((first-equal (string-find-next-char str #\=)))
            (if (not first-equal)
                attributes
            (begin 
            (display first-equal)
            (newline)
            (display str)
            (newline)
            (let* ((key (substring str 0 first-equal))
                 (value (substring str first-equal (string-find-next-char-in-set (string-tail str (string-length key)) (char-set #\\ #\ )))))
                    (begin 
                        (append! attributes (cons key value))
                        (get-attributes (string-tail str (+ (string-length key) (string-length value))) attributes)))))))
    (display (get-attributes token '())))
    ;(let* ((type (get-type token))
    ;       (attributes (get-attributes

    (make-node (get-type token) (get-attributes token))
|#
;;; Takes in a list of tokens and parses them, returning a tree
#|
(define (parser tokens)
    (define (parser-helper tokens stack tree)
        (let (first-token (car tokens))
            (cond ((start-tag? first-token)
                   (append! stack (make-

    (parser-helper tokens '() '()))
|#
