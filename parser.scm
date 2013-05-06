
;;; TODO(mikemeko): comment
(define (make-tree start? end? self-terminating? create-node-from-token)
    (define (make-tree-helper stack tokens)
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
;; TODO(mikemeko): examples?
|#

