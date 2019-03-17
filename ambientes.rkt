#lang eopl

(require "datatypes.rkt")
(require "funciones_auxiliares.rkt")

(provide (all-defined-out))

; Ambiente inicial
(define init-env
	(lambda ()
		(empty-env)
	)
)

;función que crea un ambiente vacío
(define empty-env  
	(lambda ()
		(empty-env-record)
    )
) 

;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
	(lambda (syms vals env)
		(extended-env-record syms (list->vector vals) env)
    )
)

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
	(lambda (proc-names idss bodies old-env)
		(let ((len (length proc-names)))
			(let ((vec (make-vector len)))
				(let ((env (extended-env-record proc-names vec old-env)))
					(for-each
						(lambda (pos ids body)
							(vector-set! vec pos (closure ids body env))
                        )
					(iota len) idss bodies)
				env)
            )
        )
    )
)

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
	(lambda (end)
		(let loop ((next 0))
			(if (>= next end) '()
				(cons next (loop (+ 1 next)))
            )
        )
    )
)


;función que busca un símbolo en un ambiente
(define apply-env
	(lambda (env sym)
		(deref (apply-env-ref env sym))
    )
)

(define apply-env-ref
	(lambda (env sym)
		(cases environment env
			(empty-env-record () (eopl:error 'apply-env-ref "No binding for ~s" sym) )
			(extended-env-record (syms vals env)
		        (let ((pos (rib-find-position sym syms)))
				    (if (number? pos)
					    (a-ref pos vals)
				        (apply-env-ref env sym)
                    )
                )
            )
        )
    )
)