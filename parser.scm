;;; Returns true if the argument is a the beginning of a tag, a la <p> or <div>
(define (start-tag? str)
    (string=? "#\<" (string-ref str 0)))


;;; Takes in a list of tokens and parses them, returning a tree
(define (parser tokens)
    (define (parser-helper tokens)
        #t)


    (parser-helper tokens))
