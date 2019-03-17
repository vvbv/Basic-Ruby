#lang eopl

(require "lenguaje.rkt")

(provide (all-defined-out))

;definición del tipo de dato ambiente
(define-datatype environment environment?
	(empty-env-record)
	(extended-env-record
		(syms (list-of symbol?))
		(vec vector?)
		(env environment?)
    )
)

;Procedimientos
(define-datatype procval procval?
	(closure
		(ids (list-of symbol?))
		(body expression?)
		(env environment?)
    )
)

;Referencias
(define-datatype reference reference?
	(a-ref 
        (position integer?)
		(vec vector?)
    )
)

(define deref
	(lambda (ref)
		(primitive-deref ref)
    )
)

(define primitive-deref
	(lambda (ref)
		(cases reference ref
			(a-ref (pos vec)
			    (vector-ref vec pos)
            )
        )
    )
)

(define setref!
	(lambda (ref val)
		(primitive-setref! ref val)
    )
)

(define primitive-setref!
	(lambda (ref val)
		(cases reference ref
			(a-ref (pos vec)
			    (vector-set! vec pos val)
            )
        )
    )
)


