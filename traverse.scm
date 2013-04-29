; ****************************************************
; NOTES ON TREE REPRESENTATION:

; Nodes are represented as: (cons record-type (list child1 child2)
; where child where child1 and child2 are cons pairs

; ****************************************************


; METHODS DEPENDENT STRUCTURE OF TREE

; takes in a given tree segment (list of lists and nodes)
; returns list of tree sections with roots that 
; are children of given tree section root
(define (children tree-segment)
  ; check its in proper format
  (cdr tree-segment)
)

; takes in a given a given tree segment (list of lists and nodes)
; and type returns either 
; a) list of tree sections with roots that
; match that type if one exists
; b) '*none if none exist
(define (get tree-segment type)
  '*nothing*
)

