#lang eopl

(provide (all-defined-out))

(require "lenguaje.rkt")
(require "datatypes.rkt")
(require "ambientes.rkt")
(require "funciones_auxiliares.rkt")

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
	(lambda (pgm)
		(cases ruby-program pgm
			(a-program (body)
				(eval-batch-expression body (init-env))
			)
		)
	)
)

(define eval-batch-expression
	(lambda (exp env)
		(cases exp-batch exp
			(a-batch (exp exps)
				(let loop 
					(
						(acc (eval-expression exp env))
						(exps exps)
					)
					(if (null? exps) 
						(if (not (null? acc))
							acc
							'=>nill
						)
						(loop (eval-expression (car exps) env) (cdr exps))
					)
				)
			)
		)
	)
)

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
	(lambda (exp env)
		(cases expression exp
			(lit-exp (datum) datum)
			(var-exp (id) (apply-env env id))
			(primapp-exp (prim rands)
				(let ((args (eval-rands rands env)))
					(apply-primitive prim args)
				)
			)
			(if-exp (test-exp true-exp false-exp)
				(if (true-value? (eval-expression test-exp env))
					(eval-expression true-exp env)
					(eval-expression false-exp env)
				)
			)
			(let-exp (ids rands body)
				(let ((args (eval-rands rands env)))
					(eval-expression body
					(extend-env ids args env))
				)
			)
			(proc-exp (ids body)
				(closure ids body env)
			)
			(app-exp (rator rands)
				(let 
					((proc (eval-expression rator env))
						(args (eval-rands rands env))
					)
					(if (procval? proc)
						(apply-procedure proc args)
						(eopl:error 'eval-expression "Attempt to apply non-procedure ~s" proc)
					)
				)
			)
			(print-exp (exp)
				(display (eval-expression exp env))
			)
			(letrec-exp (proc-names idss bodies letrec-body)
				(eval-expression letrec-body
					(extend-env-recursively proc-names idss bodies env)
				)
			)
			(set-exp (id rhs-exp)
				(begin
					(setref!
						(apply-env-ref env id)
						(eval-expression rhs-exp env)
					)
					1
				)
			)
			(begin-exp (exp exps) 
				(let loop 
					(
						(acc (eval-expression exp env))
						(exps exps)
					)
					(if (null? exps) 
						acc
						(loop (eval-expression (car exps) env) (cdr exps))
					)
				)
			)
		)
	)
)

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
	(lambda (rands env)
		(map (lambda (x) (eval-rand x env)) rands)
	)
)

(define eval-rand
	(lambda (rand env)
		(eval-expression rand env)
	)
)

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
	(lambda (prim args)
		(cases primitive prim
			(add-prim () (+ (car args) (cadr args)))
			(substract-prim () (- (car args) (cadr args)))
			(mult-prim () (* (car args) (cadr args)))
			(incr-prim () (+ (car args) 1))
			(decr-prim () (- (car args) 1))
		)
	)
)

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
	(lambda (proc args)
		(cases procval proc
			(closure (ids body env)
				(eval-expression body (extend-env ids args env))
			)
		)
	)
)

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
	(lambda (x)
		(not (zero? x))
	)
)



