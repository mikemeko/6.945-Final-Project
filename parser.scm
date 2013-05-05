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

;;; Takes in a token and returns an XML node
(define (create-xml-node token)
    (pp "TOKEN")
    (pp token)
    (if (and (not (self-terminating-tag? token)) (not (start-tag? token)) (not (end-tag? token)))
        (make-node 'non-tag (list (cons "text" token)))
    (let ((parameters (get-type-and-attributes token)))
         (make-node (car parameters) (cdr parameters)))))

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

(define (make-tree start? end? self-terminating? create-node-from-token)
    (define (make-tree-helper stack tokens)
        (pp "STACK")
        (pp stack)
        (if (= (length tokens) 0)
            (car stack) ;TODO (marx) make sure stack has one element
            (let ((first-token (car tokens)))
                (cond ((start? first-token)
                            (let ((new-node (new-segment (create-node-from-token first-token) '())))
                                  (make-tree-helper (append (list new-node) stack) (cdr tokens))))
                      ((end? first-token)
                            (let ((first-subtree (car stack));TODO (marx) add assertions for matching tag
                                   (second-subtree (cadr stack)))
                                       (begin (add-child second-subtree first-subtree)
                                            (make-tree-helper (append (list second-subtree) (cddr stack)) (cdr tokens)))))
                      ((self-terminating? first-token)
                            (let ((new-node (new-segment (create-node-from-token first-token) '()))
                                   (first-subtree (car stack)))
                                       (begin (set-children first-subtree (append (children first-subtree) (list new-node)))
                                            (make-tree-helper (append (list first-subtree) (cdr stack)) (cdr tokens)))))
                      (else
                            (let ((new-node (new-segment (create-node-from-token first-token) '()))
                                   (first-subtree (car stack)))
                                       (begin (set-children first-subtree (append (children first-subtree) (list new-node)))
                                        (make-tree-helper (append (list first-subtree) (cdr stack)) (cdr tokens)))))))))
    (let ((root (make-node '*the-root* '())))
        (lambda (tokens)
            (make-tree-helper (list (new-segment root '())) tokens))))
;; Examples
#|
(make-tree '("<p a=1>" "hello" "</p>"))
>(#[node 24] (#[node 25] #[node 26])) 

(get-tag (caadr (make-tree '("<p a=1>" "hello" "</p>")))) 
> "p"
|#

;; Examples
#|
(make-tree '("<p a=1>" "hello" "</p>"))
>(#[node 24] (#[node 25] #[node 26])) 

(get-tag (caadr (make-tree '("<p a=1>" "hello" "</p>")))) 
> "p"
|#

