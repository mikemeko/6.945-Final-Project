; ****************************************************
; This file is for methods dependent on the structure of the tree.
;
; NOTES ON TREE REPRESENTATION:
;
; Tree segments are represented as: (cons parent-node (list child1 child2)
; where child where child1 and child2 are cons pairs

; There are four types of nodes: tags, text, comments, and the root
; Tags have data that are a lists of cons pairs
; corresponding to attributes
; Texts+comments have corresponding text data in the same
; form (e.g. (cons "text" "Hello World!"))
;
; USER-MODEL
;
; The user interfaces exclusively with tree segments.  
; That is, any user-facing accessor will 
; take in a single tree segment and return either
; A) another single tree segment or list of tree segments
; B) very specific data (tag name, specific attribute data,
; list of attributes it has, etc) as a string, 
; list of strings, or number
;
; TODO(pauL): 
;   -inconsistent naming?

; ****************************************************

; ************************
; FOR USE BY OTHER METHODS
; ************************

; Takes in a tree segment and tag
; Returns list of tree segments whose roots
; are children with the given tag
; Returns '() if none exist
(define (filter-descendents-by-tag segment tag)
    ; Takes in a list of tree segments and a tag
    ; Returns either 
    ; a) list of tree segments with roots that
    ; match that tag if one exists
    ; b) '() if none exist
    (define (filter-list-by-tag segments tag)
      (define (compare-tag x)
        (equal? (get-tag (current x)) tag))
      (list-transform-positive segments compare-tag))
    (filter-list-by-tag (children segment) tag))

; *****************
; NODE TYPES
; ******************

; Returns #t if tag node, false otherwise
(define (tag? segment)
  (not (or (equal? (tag segment) '*the-root*)
  (equal? (tag segment) 'text)
  (equal? (tag segment) 'comment))))

; Returns #t if entire document, false otherwise
(define (root? segment) (equal? (tag segment) '*the-root*))

; Returns #t if text node, false otherwise
(define (text? segment) (equal? (tag segment) 'text))

; Returns #t if comment node, false otherwise
(define (comment? segment) (equal? (tag segment) 'comment))

; ************************************
; MODIFYING AND ACCESSING CURRENT NODE
; ************************************

; Takes in tree segment
; Returns its tag
(define (tag segment) (get-tag (current segment)))

; Takes in tree segment and tag
; Sets tag to specified tag
(define (modify-tag segment new-tag)
  (set-tag (current segment) new-tag))

; Takes in tree segment
; Returns a list of its attributes (not values) its root
(define (attributes segment)
  (assert (tag? segment) "only tags have attributes")
  (map (lambda (x) (car x)) (get-data (current segment))))

; Takes in tree segment and attributes
; Returns #t if tree segment root has that attribute
(define (attribute? segment attribute)
  (if (find (lambda(x) (equal? (car x) attribute)) (get-data (current segment))) #t #f))

; Takes in tree segment and attribute
; Returns value of specified attribute 
(define (get-attribute segment attribute)
  (assert (tag? segment) "only tags have attributes")
  (assert (attribute? segment attribute) "doesn't have attribute")
  (cdr (find (lambda(x) (equal? (car x) attribute)) (get-data (current segment)))))

; Removes attribute if it exists
; Does nothing otherwise
(define (remove-attribute segment attribute)
  (if (attribute? segment attribute)
  (set-data (current segment)
    (remove (lambda(x) (equal? (car x) attribute)) 
      (get-data (current segment))))))

; Takes in tree segment, attribute, and value
; Either adds or modifies that particular attribute to the new value
; Note: This potentially changes the order of the attributes
; but that doesn't matter (according to the XML spec)
(define (set-attribute segment attribute new-value)
  (remove-attribute segment attribute)
  (set-data (current segment) (append (get-data (current segment)) 
      (list (cons attribute new-value)))))

; Takes in a tree segment whose root is a text node
; Returns text
(define (get-text segment)
  (assert (text? segment) "not at text node")
  (cdr (find (lambda(x) (equal? (car x) "text")) (get-data (current segment)))))

; Takes in a tree segment whose root is a comment node
; Returns comment 
(define (get-comment segment)
  (assert (text? segment) "not a comment node")
  (cdr (find (lambda(x) (equal? (car x) "comment")) (get-data (current segment)))))

; Takes in tree segment
; Returns how many children it has
(define (count segment)
  (length (children segment)))

; ************************************
; TREE TRAVERSAL
; ************************************

; Takes in a tree segment and pos
; Returns pos'th child segment
(define (walk segment pos)
  (list-ref (children segment) pos))

; Takes in tree segment and tag
; Returns how many child segments that has a root
; of the given tag
(define (count-tag segment tag)
  (let ((filtered-descendents (filter-descendents-by-tag segment tag)))
    (length filtered-descendents)))

; Takes in tree segment, tag, and position
; Returns the pos'th child segment that has a root
; of the given tag
; (i.e. (get "head" html 0) (get "channel" rss 0)
(define (walk-by-tag segment tag pos)
  (let ((filtered-descendents (filter-descendents-by-tag segment tag)))
  (assert (> (length filtered-descendents) pos) "out-of-range")
        (list-ref filtered-descendents pos)))

; ************************
; SEGMENT CONSTRUCTORS
; ************************

; Takes in children
; Returns root tree segment with those children
(define (new-root children)
 (new-segment (make-node '*the-root* '()) children)
)

; Takes in tag, attributes, and children
; Returns tag tree segment with those properties
(define (new-tag tag attributes children)
  (new-segment (make-node tag attributes) children)
)

; Takes in text
; Returns text tree segment 
(define (new-text text)
  (new-segment (make-node 'text (list (cons "text" text))) '())
)

; Takes in text
; Returns comment tree segment 
(define (new-comment text)
  (new-segment (make-node 'comment (list (cons "comment" text))) '())
)

; ************************
; STRINGIFICATION + OUTPUT
; ************************

; Takes in tree segment and attribute
; Returns string representation of attribute and value
(define (stringify-attribute segment attribute)
  (string-append attribute "=" (get-attribute segment attribute)))

; Takes in tag tree segment
; Returns string representation of attributes
; and corresponding values
(define (stringify-attributes segment)
  (assert (tag? segment) "Not a tag segment")
  (apply string-append (map 
    (lambda(x) (string-append " " (stringify-attribute segment x))) 
    (attributes segment))))

; Takes in tag tree segment
; Returns string representation of opening tag
(define (stringify-opening segment)
  (assert (tag? segment) "Not a tag segment")
  (string-append "<" (tag segment) 
    (stringify-attributes segment) ">"))

; Takes in tag tree segment
; Returns string representation of closing tag
(define (stringify-closing segment)
  (assert (tag? segment) "Not a tag segment")
  (string-append "</" (tag segment) ">"))

; Takes in comment segment
; Returns opening comment string
(define (stringify-children segment)
  (apply string-append 
      (map (lambda (x) (stringify x)) (children segment))))

(define (stringify-comment-opening segment)
  (assert (comment? segment) "Not a comment segment")
  "<!--")

; Takes in comment tree segment
; Returns closing comment string
(define (stringify-comment-closing segment)
  (assert (comment? segment) "Not a comment segment")
  "-->")


; Takes in tree segment
; Returns tree segment as string of XML
(define (stringify segment)
  (cond ((tag? segment) (string-append 
            (stringify-opening segment)
            (stringify-children segment) 
            (stringify-closing segment)))
        ((root? segment) 
            (stringify-children segment))
        ((comment? segment) (string-append
            (stringify-comment-opening segment)
            (get-comment segment)
            (stringify-comment-closing segment)))
        (else (get-text segment))))

; Takes in segment and filename
; Creates/overwrites file with corresponding XML
(define (write-tree segment filename)
  (write-to-file filename (stringify segment)))

