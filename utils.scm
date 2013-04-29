;; Utility methods

;;; asserts that |thing-that-should-be-true| is indeed true, or throws an error
;;;     displaying the given |message|
(define (assert thing-that-should-be-true message)
  (if (not thing-that-should-be-true)
      (error message)))

