;; Utility methods

(define RUN-TESTS #t)

;;; asserts that |thing-that-should-be-true| is indeed true, or throws an error
;;;     displaying the given |message|
(define (assert thing-that-should-be-true message)
  (if (not thing-that-should-be-true)
      (error message)))

;;; Returns #t if |text| starts with |prefix|, #f otherwise
(define (starts-with text prefix)
  (let ((text-n (string-length text))
        (prefix-n (string-length prefix)))
    (if (>= text-n prefix-n)
        (string=? (string-head text prefix-n) prefix)
        #f)))

;; Examples
#|
(starts-with "abc" "a")
> #t
(starts-with "abc" "ab")
> #t
(starts-with "abc" "d")
> #f
(starts-with "abc" "abcd")
> #f
|#

;;; Returns #t if |text| ends with |suffix|, #f otherwise
(define (ends-with text suffix)
  (let ((text-n (string-length text))
        (suffix-n (string-length suffix)))
    (if (>= text-n suffix-n)
        (string=? (string-tail text (- text-n suffix-n)) suffix)
        #f)))

;; Examples
#|
(ends-with "abc" "c")
> #t
(ends-with "abc" "bc")
> #t
(ends-with "abc" "d")
> #f
(ends-with "abc" "dabc")
> #f
|#

