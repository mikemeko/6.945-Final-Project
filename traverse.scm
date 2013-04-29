
; METHODS DEPENDENT STRUCTURE OF TREE

; takes in a given tree section (list of lists and nodes)
; returns list of tree sections with roots that 
; are children of given tree section root
(define children (node)

)

; takes in a given a given tree section (list of lists and nodes)
; and type returns either 
; a) list of tree sections with roots that
; match that type if one exists
; b) '*none if none exist
(define get (node type)

)

; METHODS INDEPENDENT OF TREE STRUCTURE

; takes in a given node
; returns type of node
(define get-type (node)

  ; wrap around define-structure

)

; takes in node and type
; replaces node's current type with new type
(define set-type (node type)

  ; wrap around define-structure

)

; takes in a given node
; returns list of attributes of node
(define get-attr (node)

  ; wrap around define-structure
  
)

; takes in node and list of attributes in form ("..", "..")
; replaces current attributes of node with new list
(define set-attr (node attrs)

  ; wrap around define-structure

)

