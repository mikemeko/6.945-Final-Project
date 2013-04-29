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
