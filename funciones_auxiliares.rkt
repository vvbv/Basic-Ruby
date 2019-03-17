#lang eopl

(provide (all-defined-out))

;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente
(define rib-find-position 
	(lambda (sym los)
		(list-find-position sym los)
    )
)

(define list-find-position
	(lambda (sym los)
		(list-index (lambda (sym1) (eqv? sym1 sym)) los)
    )
)

(define list-index
	(lambda (pred ls)
		(cond
			((null? ls) #f)
			((pred (car ls)) 0)
			(else 
                (let ((list-index-r (list-index pred (cdr ls))))
					(if (number? list-index-r)
					(+ list-index-r 1)
				    #f)
                )
            )
        )
    )
)