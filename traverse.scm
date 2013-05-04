; ****************************************************
; This file is for methods dependent on the structure of the tree.
;
; NOTES ON TREE REPRESENTATION:
;
; Tree segments are represented as: (cons parent-node (list child1 child2)
; where child where child1 and child2 are cons pairs

; There are three types of nodes: tags, non-tags, and the root
; Tags have data that are a lists of cons pairs
; corresponding to attributes
; Non-tags have corresponding text data in the same
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
; TODO(pauL): fix inconsistent naming?

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
        (equal? (get-tag (car x)) tag))
      (list-transform-positive segments compare-tag))
    (filter-list-by-tag (cdr segment) tag))

; Checks tree segment representation
(define (check-rep segment)
  ; currently quick, very non-comprehensive check of input format
  (assert (list? segment) "input should be a tree segment")
  (define (list-or-node? x)
    (assert (or (node? x) (list? x)) "input should be a tree segment"))
  (for-each list-or-node? segment))

; *****************
; NODE TYPES
; ******************

(define (tag? segment)
  (not (or (equal? (tag segment) '*the-root*)
  (equal? (tag segment) 'non-tag))))

(define (root? segment) (equal? (tag segment) '*the-root*))

(define (text? segment) (equal? (tag segment) 'non-tag))


; ************************************
; MODIFYING AND ACCESSING CURRENT NODE
; ************************************

(define (tag segment) (get-tag (car segment)))

(define (modify-tag segment new-tag)
  (set-tag (car segment) new-tag))

; returns list of attributes (not values)
(define (attributes segment)
  (assert (tag? segment) "only tags have attributes")
  (map (lambda (x) (car x)) (get-data (car segment))))

(define (attribute? segment attribute)
  (if (find (lambda(x) (equal? (car x) attribute)) (get-data (car segment))) #t #f))

(define (get-attribute segment attribute)
  (assert (tag? segment) "only tags have attributes")
  (assert (attribute? segment attribute) "doesn't have attribute")
  (cdr (find (lambda(x) (equal? (car x) attribute)) (get-data (car segment)))))

; Removes attribute if it exists
; Does nothing otherwise
(define (remove-attribute segment attribute)
  (if (attribute? segment attribute)
  (set-data (car segment)
    (remove (lambda(x) (equal? (car x) attribute)) 
      (get-data (car segment))))))

; Takes in tree segment, attribute, and value
; Either adds or modifies that particular attribute to the new value
; Note: This potentially changes the order of the attributes
; but that doesn't matter (according to the XML spec)
(define (set-attribute segment attribute new-value)
  (remove-attribute segment attribute)
  (set-data (car segment) (append (get-data (car segment)) 
      (list (cons attribute new-value)))))

(define (get-text segment)
  (assert (text? segment) "not at text node")
  (cdr (find (lambda(x) (equal? (car x) "text")) (get-data (car segment)))))

; Takes in tree segment
; Returns how many children it has
(define (count segment)
  (length (cdr segment)))

; ************************************
; TREE TRAVERSAL
; ************************************

; Takes in a tree segment and pos
; Returns pos'th child segment
(define (walk segment pos)
  (list-ref (cdr segment) pos))

; Takes in a tree segment and pos
; Returns list of child segments
(define (children segment)
  (cdr segment))

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
; STRINGIFICATION + OUTPUT
; ************************

(define (stringify-attribute segment attribute)
  (string-append attribute "=" (get-attribute segment attribute)))

(define (stringify-attributes segment)
  (apply string-append (map 
    (lambda(x) (string-append " " (stringify-attribute segment x))) 
    (attributes segment))))

(define (stringify-opening segment)
  (string-append "<" (tag segment) 
    (stringify-attributes segment) ">"))

(define (stringify-closing segment)
  (string-append "</" (tag segment) ">"))

(define (stringify-children segment)
  (if (eq? (count segment) 0)
      ""
      (apply string-append 
        (map (lambda (x) (stringify x)) (children segment)))))

(define (stringify segment)
  (cond ((tag? segment) (string-append 
            (stringify-opening segment)
            (stringify-children segment) 
            (stringify-closing segment)))
        ((root? segment) 
            (stringify-children segment))
        (else (get-text segment))))

(define (write-tree segment filename)
  (write-to-file filename (stringify segment)))

