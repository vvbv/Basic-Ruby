#lang eopl
(provide 
    extra:say-hello 
    extra:get-pos-first-list
    extra:list-set!
    extra:put-elements-in-list
    extra:remove
    extra:display
    extra:displayln
    extra:exist-in?
    extra:binary-tree-to-list
    extra:insert-in-list
    extra:exist-in-list
    extra:remove-specific-position
    extra:remove-duplicates
    extra:cut-list
)

; Function that says Hello
(define extra:say-hello
    (lambda ()
        (display "Hello")
    )
)

; Function that return the position of the first element of type list
; param: list
; param: position → Counter
; return: list

(define extra:low-get-pos-first-list
    (lambda (lst position non-empty?)
        (cond
            [(null? lst) -1 ]
            [else 
                (cond
                    [(list? (car lst))
                        (if non-empty?
                            (begin
                                (cond
                                    [(not (null? (car lst)))
                                        position
                                    ]
                                    [else
                                        (extra:low-get-pos-first-list (cdr lst) (+ 1 position) non-empty?)
                                    ]
                                )
                            )
                            position
                        )
                    ]
                    [ else 
                        (extra:low-get-pos-first-list (cdr lst) (+ 1 position) non-empty?)
                    ]
                )
            ]
        )
    )
)

; Function that return the position of the first element of type list
; See: extra:low-get-pos-first-list
; param: list
; return: list

(define extra:get-pos-first-list
    (lambda (lst non-empty?)
        (extra:low-get-pos-first-list lst 0 non-empty? )
    )
)

; Function that replace an specific element in a list.
; param: list
; param: position
; param: new-value
; return: list

(define extra:list-set! 
    (lambda (lst position new-value)
        (define new-list (list))
        (for-each 
            (lambda (element)
                (if (eq? position 0)
                    (begin
                        (set! new-list (append new-list (list new-value)))
                        (set! position (- position 1))
                    )
                    (begin
                        (set! new-list (append new-list (list element)))
                        (set! position (- position 1))
                    )
                )
            )
            lst
        )
        new-list
    )
)

; Function that pun an specific position in a list, a list of element like append.
; param: list
; param: position
; param: list-of-elements
; param: replace? #t → remove and put in that location the new elements. #f → Move whole the list to right and insert the new elements.
; return: list

(define extra:put-elements-in-list
    (lambda (lst position list-of-elements replace?)
        (define new-list (list))
        (for-each 
            (lambda (element)
                (if (eq? position 0)
                    (begin
                        (if replace? 
                            (set! new-list (append new-list list-of-elements))
                            (begin
                                (set! new-list (append new-list list-of-elements))
                                (set! new-list (append new-list (list element)))
                            )
                        )
                        (set! position (- position 1))
                    )
                    (begin
                        (set! new-list (append new-list (list element)))
                        (set! position (- position 1))
                    )
                )
            )
            lst
        )
        (if (= position 0)
            (begin
                (set! new-list (append new-list list-of-elements))
                (set! position -1)
            )
            #f
        )
        
        new-list 
    )
)

; Function that remove from a list, every element that coincide with the condition.
; param: lst
; param: element-to-remove
; return: list

(define extra:remove
    (lambda (lst element-to-remove)
        (define new-list (list))
        (for-each
            (lambda (element)
                (cond 
                    [(not (eq? element element-to-remove))
                        (set! new-list (append new-list (list element) ) )
                    ]
                )
            )
            lst
        )
        new-list
    )
)

; Function that display multiple inputs.
; param: any
; return: void

(define extra:display
    (lambda input
        (cond
            [(list? input)
                (for-each
                    (lambda (arg)
                        (display arg)
                    )
                    input
                )
            ]
            [else
                (display input)
            ]
        )
    )
)

; Function that display multiple inputs ending with a new line.
; param: any
; return: void

(define extra:displayln
    (lambda input
        (cond
            [(list? input)
                (for-each
                    (lambda (arg)
                        (display arg)
                    )
                    input
                )
            ]
            [else
                (display input)
            ]
        )
        (newline)
    )
)
; Funtion that return #t if in an list exist an element of type flag
; param: lst
; param: comparator → ie. list?

(define extra:exist-in?
    (lambda (lst flag)      
        (if (null? lst)
            #f
            (begin 
                (if (flag (car lst))
                    #t
                    (extra:exist-in? (cdr lst) flag)
                )
            )
        )  
    )
)

; Funtion that cast an binary-tree in a list keepeing the order
; param: b-tree
; param: stack
; return: list

(define extra:low-binary-tree-to-list
    (lambda (b-tree stack)
        (cond
            [(and (not (null? b-tree)) (null? stack))
                (set! stack 
                    (list 
                        (car (cdr b-tree)) 
                        (car b-tree) 
                        (car (cddr b-tree)) 
                    )
                )
                (extra:low-binary-tree-to-list b-tree stack)
            ]
            [(extra:exist-in? stack list?)
                (let ([pos (extra:get-pos-first-list stack #t)][new-stack-element (list)])
                    (cond 
                        [(> pos -1)
                            (begin 
                                (if (= (length (list-ref stack pos)) 3)
                                    (if (not (null? (car (cdr (list-ref stack pos)))))
                                        (set! new-stack-element (append new-stack-element (car (cdr (list-ref stack pos)))))
                                        #t
                                    )
                                    #t
                                )
                                (set! new-stack-element (append new-stack-element (list (car (list-ref stack pos)))))
                                (if (= (length (list-ref stack pos)) 3)
                                    (if (not (null? (car (cddr (list-ref stack pos)))))
                                        (set! new-stack-element (append new-stack-element (cddr (list-ref stack pos))))
                                        #t
                                    )
                                    #t
                                )
                                (set! stack (extra:put-elements-in-list stack pos new-stack-element #t))
                                ;
                            )
                        ]
                    )
                )
                (set! stack (extra:remove stack (list)))
                (extra:low-binary-tree-to-list b-tree stack)
            ]
            [(not (extra:exist-in? stack list?))
                stack
            ]
        )
    )
)

; See: extra:low-binary-tree-to-list

(define extra:binary-tree-to-list
    (lambda (b-tree)
        (extra:low-binary-tree-to-list b-tree (list))
    )
)

; Function that insets into list an element
; param: lst
; param: element
; param: only-if-no-exist → #t insert the element only if no exist, → #f insert witout validation if exist
; return: list

(define extra:insert-in-list
    (lambda (lst new-element only-if-no-exist)
        (case only-if-no-exist
            [(#t)
                (let ([exist #f])
                    (for-each
                        (lambda (arg)
                            (cond 
                                [(equal? arg new-element)
                                    (set! exist #t)
                                ]
                            )
                        )
                        lst
                    )
                    (cond
                        [(not exist)
                            (set! lst (append lst (list new-element) ))
                        ]
                    )
                )
            ]
            [(#f)
                (set! lst (append lst (list new-element) ))
            ]
        )
        lst
    )
)

; Function that return #f or #f is an element exist into a list.
; param: lst
; param: element
; return: boolean

(define extra:exist-in-list
    (lambda (lst element)
        (define exist #f)
        (for-each
            (lambda (arg)
                (cond 
                    [(equal? arg element)
                        (set! exist #t)
                    ]
                )
            )
            lst
        )
        exist
    )
)

; Function that remove an item in a specific position in a list.
; param: lst
; param: pos
; return: list

(define extra:remove-specific-position
    (lambda (lst pos)
        (define to-return (list))
        (let ([counter 0])
            (for-each
                (lambda (arg)
                    (if (= counter pos)
                        #f
                        (set! to-return (append to-return (list arg)))
                    )
                    (set! counter (+ 1 counter))
                )
                lst
            )
        )
        to-return
    )
)

; Function that remove dulicates in a list.
; param: lst
; return: list

(define extra:remove-duplicates
    (lambda (lst)
        (define to-return (list))
        (for-each
            (lambda (arg)
                (set! to-return (extra:insert-in-list to-return arg #t ))
            )
            lst
        )
        to-return
    )
)

; Function that slices a list.
; param: start-pos
; param: end-pos
; param: lst
; return: list

(define extra:cut-list 
    (lambda (start-pos end-pos lst)
        (define counter 0)
        (define new-lst (list))
        (for-each
            (lambda (arg)
                (if (and (or (= counter start-pos) (> counter start-pos) ) (or (= counter end-pos) (< counter end-pos) ))
                    (set! new-lst (append new-lst (list arg)))
                    #f
                )
                (set! counter (+ 1 counter))
            )
            lst
        )
        new-lst
    )
)