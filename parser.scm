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


;;; Creates a tree from a list to tokens as created by the tokenizer
#|
(define (make-tree tokens)
    (define (make-tree-helper stack tokens)
        (if (= (length tokens) 0)
            (car stack) ;TODO (marx) make sure stack has one element
            (let ((first-token (car tokens)))
                (cond ((start-tag? first-token)
                            (let* ((parameters (get-type-and-attributes first-token))
                                   (new-node (make-node (car parameters) (cdr parameters))))
                                       (make-tree-helper (append (list (list new-node)) stack) (cdr tokens))))
                      ((end-tag? first-token)
                            (let* ((first-subtree (car stack));TODO (marx) add assertions for matching tags
                                   (second-subtree (cadr stack))
                                   (first-second-combined (append second-subtree (list first-subtree))))
                                        (make-tree-helper (append (list first-second-combined) (cddr stack)) (cdr tokens))))
                      ((self-terminating-tag? first-token)
                            (let* ((parameters (get-type-and-attributes first-token))
                                   (new-node (make-node (car parameters) (cdr parameters)))
                                   (first-subtree (car stack))
                                   (new-first-combined (append first-subtree (list new-node))))
                                        (make-tree-helper (append (list new-first-combined) (cdr stack)) (cdr tokens))))
                      (else
                            (let* ((new-node (make-node 'non-tag (list (cons "text" first-token))))
                                   (first-subtree (car stack))
                                   (new-first-combined (append first-subtree (list (list new-node)))))
                                        (make-tree-helper (append (list new-first-combined) (cdr stack)) (cdr tokens))))))))
    (let ((root (make-node '*the-root* '())))
        (make-tree-helper (list (list root)) tokens )))
|#
(define (make-tree tokens)
    (define (make-tree-helper stack tokens)
        (pp "STACK")
        (pp stack)
        (if (= (length tokens) 0)
            (car stack) ;TODO (marx) make sure stack has one element
            (let ((first-token (car tokens)))
                (pp "TOKEN")
                (pp first-token)
                (cond ((start-tag? first-token)
                            (let* ((parameters (get-type-and-attributes first-token))
                                   (new-node (new-segment (make-node (car parameters) (cdr parameters)) '())))
                                       (make-tree-helper (append stack (list new-node)) (cdr tokens))))
                      ((end-tag? first-token)
                            (let* ((first-subtree (list-ref stack (- (length stack) 1)));TODO (marx) add assertions for matching tag
                                   (second-subtree (list-ref stack (- (length stack) 2))))
                                       (begin (add-child second-subtree first-subtree)
                                            (make-tree-helper (append (list-head stack (- (length stack) 2)) (list second-subtree)) (cdr tokens)))))
                      ((self-terminating-tag? first-token)
                            (let* ((parameters (get-type-and-attributes first-token))
                                   (new-node (new-segment (make-node (car parameters) (cdr parameters)) '()))
                                   (first-subtree (list-ref stack (- (length stack) 1))))
                                       (begin (set-children first-subtree (append (children first-subtree) (list new-node)))
                                            (make-tree-helper (append (list-head stack (- (length stack) 1)) (list first-subtree)) (cdr tokens)))))
                      (else
                            (let* ((new-node (new-segment (make-node 'non-tag (list (cons "text" first-token))) '()))
                                   (first-subtree (list-ref stack (- (length stack) 1))))
                                       (begin (set-children first-subtree (append (children first-subtree) (list new-node)))
                                        (make-tree-helper (append (list-head stack (- (length stack) 1)) (list first-subtree)) (cdr tokens)))))))))
    (let ((root (make-node '*the-root* '())))
        (make-tree-helper (list (new-segment root '())) tokens )))
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

