;;; Returns a procedure that, given a list of tokens, produces a tree.
;;; This procedure works as follows. We keep a stack of found starting tags. When
;;; we encounter an ending tag, we pop off the last element (which should be the matching
;;; start tag), and add it as a child to the next element on the stack. Text/comment/self-terminating elements are treated as
;;; ending tags that do not require popping the last element of the stack. At the end of the execution
;;; every starting tag should have found a matching ending tag, and the only element on the
;;; stack will the be the tree.
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
