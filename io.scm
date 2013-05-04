;; File IO

;;; Returns a string with the content of the file with the given |file-name|.
(define (read-file file-name)
  (let ((text ""))
    (define (append-char char)
      (set! text (string-append text (string char))))
    (call-with-input-file file-name
      (lambda (input-port)
        (let loop ((x (read-char input-port)))
          (if (not (eof-object? x))
              (begin (append-char x)
                     (loop (read-char input-port)))))))
    text))

;;; Writes |text| to a file with the given |file-name|.
(define (write-to-file file-name text)
  (call-with-output-file file-name
    (lambda (output-port)
      (display text output-port))))

