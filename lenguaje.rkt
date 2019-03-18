#lang eopl

(provide (all-defined-out))

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <a-program (exp)>
;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expresion> then <expresion> else <expression>
;;                      <if-exp (exp1 exp2 exp23)>
;;                  ::= let {<identifier> = <expression>}* in <expression>
;;                      <let-exp (ids rands body)>
;;                  ::= proc({<identificador>}*) <expression>
;;                      <proc-exp (ids body)>
;;                  ::= (<expression> {<expression>}*)
;;                      <app-exp proc rands>
;;                  ::= letrec  {identifier ({identifier}*(,)) = <expression>}* in <expression>
;;                     <letrec-exp(proc-names idss bodies bodyletrec)>
;;                  ::= begin <expression> {; <expression>}* end
;;                     <begin-exp (exp exps)>
;;                  ::= set <identifier> = <expression>
;;                     <set-exp (id rhsexp)>
;;  <primitive>     ::= + | - | * | add1 | sub1 

;Especificación Léxica
(define scanner-spec-simple-interpreter
	'(
		(white-sp (whitespace) skip) 
		(comment ("#" (arbno (not #\newline))) skip) 
		(identifier ((arbno "@") letter (arbno (or letter digit "_" "?" "="))) symbol) 
		(number (digit (arbno digit)) number) 
		(number ("-" digit (arbno digit)) number) 
		(text ("\"" 
			(or letter whitespace) 
			(arbno (or letter digit whitespace ":" "?" "=" "'") ) "\"" ) 
		string) 
	)
)

;Especificación Sintáctica (gramática)
(define grammar-simple-interpreter
	'(
		(ruby-program ("ruby" exp-batch "end") a-program)
  		(exp-batch (expression (arbno expression)) a-batch)
		(expression ("declare" identifier (arbno "," identifier) ";") declare-exp)
		(expression (number) lit-exp)
		(expression (identifier) var-exp)
		(expression
			(primitive "(" (separated-list expression ",")")")
		primapp-exp)
		(expression 
            	("if" expression "then" expression "else" expression)
		if-exp)
		(expression 
            	("let" (arbno identifier "=" expression) "in" expression)
		let-exp)
		(expression 
            	("proc" "(" (arbno identifier) ")" expression)
		proc-exp)
		(expression 
            	( "(" expression (arbno expression) ")")
		app-exp)
		(expression 
            	("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)  "in" expression) 
		letrec-exp)
		(expression 
            	("begin" expression (arbno ";" expression) "end")
		begin-exp)
		(expression 
            	("print" expression )
		print-exp)
		(expression ("set" identifier "=" expression) set-exp)
		(primitive ("+") add-prim)
		(primitive ("-") substract-prim)
		(primitive ("*") mult-prim)
		(primitive ("add1") incr-prim)
		(primitive ("sub1") decr-prim)
    )
)

;Construcción de datatypes:
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)