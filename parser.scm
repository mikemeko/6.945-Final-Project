;;; TODO(mikemeko): comment
(define (make-tree start? end? create-node-from-token)
    (define (make-tree-helper stack tokens)
        (if (= (length tokens) 0)
            (begin 
            (assert (= (length stack) 1) "Stack does not have one element!")
            (car stack))
            (let ((first-token (car tokens)))
                (cond ((start? first-token)
                            (let ((new-node (new-segment (create-node-from-token first-token) '())))
                                  (make-tree-helper (append (list new-node) stack) (cdr tokens))))
                      ((end? first-token)
                            (let ((first-subtree (car stack))
                                   (second-subtree (cadr stack)))
                                            (add-child second-subtree first-subtree)
                                            (make-tree-helper (append (list second-subtree) (cddr stack)) (cdr tokens))))
                      (else
                            (let ((new-node (new-segment (create-node-from-token first-token) '()))
                                   (first-subtree (car stack)))
                                       (begin (add-child first-subtree new-node)
                                        (make-tree-helper (append (list first-subtree) (cdr stack)) (cdr tokens)))))))))
    (let ((root (make-node '*the-root* '())))
        (lambda (tokens)
            (make-tree-helper (list (new-segment root '())) tokens))))
;; Examples
#|
;; TODO(mikemeko): examples?
|#

