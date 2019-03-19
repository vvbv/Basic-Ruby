#lang eopl
;;-------------------------------------------------------------;;
;; Asignatura: Fundamentos de Lenguajes de Programación (FLP)  ;;
;; Proyecto: Implementando un Ruby básico usando SLLGEN        ;;
;; Docente: Jesus Alexander Aranda Bueno                       ;;
;; Integrantes:                                                ;;
;;      - Diana Marcela Garcia Correa  - 1531722               ;;
;;      - Juan Felipe Orozco Escobar   - 1426244               ;;
;;      - Jeison Cardona Gomez         - 1325562               ;;
;;-------------------------------------------------------------;;
;******************************************************************************************
;;;;; Interpretador para lenguaje RUBY sin objetos

;******************************************************************************************

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
		(expression (simple-exp) a-simple-exp)
		(expression ("declare" identifier (arbno "," identifier) ";") declare-exp)
		(expression ("puts" comp-value (arbno "," comp-value)) puts-exp)
		(expression ("if" comp-value (arbno "then") exp-batch (arbno "elseif" comp-value (arbno "then") exp-batch) (arbno "else" exp-batch) "end") if-exp)
		(expression ("unless" comp-value (arbno "then") exp-batch (arbno "else" exp-batch) "end") unless-exp)
		(expression ("while" comp-value (arbno "do") exp-batch "end") while-exp)
		(expression ("until" comp-value (arbno "do") exp-batch "end") until-exp)
		(expression ("for" identifier "in" comp-value (arbno "do") exp-batch "end") for-exp)
		(expression ("def" identifier "(" (arbno identifier ",") ")" exp-batch "end") function-exp)
		(expression ("super" identifier arguments ";") super-exp)
		(class-decl ("class" identifier (arbno "<" identifier) "attr" (separated-list ":" identifier ",") ";" (arbno method-decl) "end") class-exp)
		(method-decl ("def" identifier "(" (separated-list identifier ",") ")" exp-batch "end") a-method-decl)
		(simple-exp (simple-value complement ";") val-exp)
		(complement ("=" comp-value calls) assign)
		(complement (assign-op comp-value calls) assign-and)
		(complement (calls) comp-calls)
		(simple-value (identifier) id-val)
		(simple-value (number) num-val)
		(simple-value (text) str-val)
		(simple-value ("true") true-val)
		(simple-value ("false") false-val)
		(simple-value ("nil") nil-val)
		(simple-value ("[" (arbno comp-value ",") "]") arr-val)
		(comp-value (value) op-value)
		(comp-value (un-op comp-value) unop-value)
		(value (simple-value) simple-val)
		(value ("(" value val-compl ")") call-val)
		(val-compl (calls) val-call)
		(val-compl (bin-op value) binop-val)
		(call ("." identifier arguments) method-call)
		(call (arguments) arguments-call)
		(calls ((arbno call)) some-calls)
		(arguments ("(" (separated-list comp-value ",") ")") m-arguments)
		(arguments ("[" comp-value (arbno "," comp-value) "]") arr-arguments)
		(un-op ("not") not-op)
		(un-op ("!") not-op)
		(bin-op ("+") add)
		(bin-op ("-") diff)
		(bin-op ("*") mult)
		(bin-op ("/") div)
		(bin-op ("%") mod)
		(bin-op ("**") pow)
		(bin-op (">") great)
		(bin-op (">=") great-eq)
		(bin-op ("<") less)
		(bin-op ("<=") less-eq)
		(bin-op ("==") is-equal)
		(bin-op ("!=") not-equal)
		(bin-op ("and") and-op)
		(bin-op ("&&") and-op)
		(bin-op ("or") or-op)
		(bin-op ("||") or-op)
		(bin-op ("..") in-range)
		(bin-op ("...") ex-range)
		(bin-op ("step") st-range)
		(assign-op ("+=") add-eq)
		(assign-op ("-=") diff-eq)
		(assign-op ("*=") mult-eq)
		(assign-op ("/=") div-eq)
		(assign-op ("**=") pow-eq)
    )
)

;Tipos de datos para la sintaxis abstracta de la gramática
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-ruby-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-ruby-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-ruby-program
  (lambda (pgm)
    (cases ruby-program pgm
      (a-program (body)
                 (eval-batch-expression body (init-env))))))

;eval-batch-expression: Evalúa la última expresión
(define eval-batch-expression
  (lambda (exp env)
    (cases exp-batch exp
      (a-batch (exp exps)
               (eval-ruby-expression exp exps env))
        )))

;eval-ruby-expression:evalua la expresión en el ambiente de entrada
(define eval-ruby-expression
  (lambda (exp exps env)
    (cases expression exp
      (a-simple-exp (exp) exp)
      (declare-exp (id idss) idss)
      (puts-exp (val vals) vals)
      (if-exp (val exp1 val2 exp2 exp3) val)
      (unless-exp (val exp1 exp2) val)
      (while-exp (val exp) val)
      (until-exp (val exp) val)
      (for-exp (id val exp) id)
      (function-exp (id ids exp) id)
      (super-exp (id args) id)
      )))

;eval-simple-expression:evalua una expresión simple dentro de una expresión
(define eval-simple-expression
  (lambda (s-exp env)
    (cases simple-exp s-exp
      (val-exp (val comp) val)
      )))

;eval-complement:evalua una complemento dentro de una expresión
(define apply-complement
  (lambda (s-val compl env)
    (cases complement compl
      (assign (val calls) val)
      (assign-and (as-op val calls) val)
      (comp-calls (calls) calls)
      )))

;eval-calls:
(define apply-call-list
  (lambda (value c-list env)
    (cases calls exp
      (some-calls (calls) calls)
      )))

;eval-call:
(define apply-call
  (lambda (value a-call env)
    (cases call exp
      (method-call (id args) id)
      (arguments-call (args) args)
      )))

;eval-arguments:
(define eval-arguments
  (lambda (args env)
    (cases arguments args
      (m-arguments (vals) vals)
      (arr-arguments (val vals) val)
      )))

;eval-comp-value:
(define eval-comp-value
  (lambda (c-val env)
    (cases comp-value c-val
      (op-value (val) val)
      (unop-value (un-op val) un-op)
      )))

;eval-value:
(define eval-value
  (lambda (a-val env)
    (cases value a-val
      (simple-val (val) val)
      (call-val (val val1) val)
      )))

;eval-val-compl:
(define eval-val-compl
  (lambda (a-val a-v-compl env)
    (cases val-compl a-v-compl
      (val-call (calls) calls)
      (binop-val (bin-op val) bin-op)
      )))

;eval-simple-value:
(define eval-simple-value
  (lambda (s-val env)
    (cases simple-value s-val
      (id-val (id) id)
      (num-val (num) num)
      (str-val (t) t)
      (true-val () "ok")
      (false-val () "ok")
      (nil-val () "ok")
      (arr-val (val) val)
      )))

;apply-bin-op:
(define eval-bin-op
  (lambda (op arg1 arg2)
    (cases bin-op op
      (add () "ok")
      (diff () "ok")
      (mult () "ok")
      (div () "ok")
      (mod () "ok")
      (pow () "ok")
      (great () "ok")
      (great-eq () "ok")
      (less () "ok")
      (less-eq () "ok")
      (is-equal () "ok")
      (not-equal () "ok")
      (and-op () "ok")
      (or-op () "ok")
      (in-range () "ok")
      (ex-range () "ok")
      (st-range () "ok")
      )))

;apply-assign-op:
(define apply-assign-op
  (lambda (as-op var-val a-val)
    (cases assign-op as-op
      (add-eq () "ok")
      (diff-eq () "ok")
      (mult-eq () "ok")
      (div-eq () "ok")
      (pow-eq () "ok")
      )))

;apply-un-op:
(define apply-un-op
  (lambda (op args)
    (cases un-op op
      (not-op () "ok")
      )))

;*******************************************************************************************
;Datatypes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expression?))
                                   (env environment?)))

(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

;*******************************************************************************************
;Ambientes

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 

; Ambiente inicial
;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z)
;     '(4 2 5)
;     (empty-env))))
(define init-env
  (lambda ()
    (extend-env
     '(x y)
     (list 4 2) (empty-env))))

;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define extend-env-recursively
  (lambda (proc-names idss bodies env)
    (recursively-extended-env-record proc-names idss bodies env))) 

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (closure (list-ref idss pos)
                                                      (list-ref bodies pos) env)
                                             (apply-env old-env sym))))
      )))

;*******************************************************************************************
; Funciones auxiliares
;Función para aplicar eval-expression a cada elemento de una lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-ruby-expression rand env)))

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-ruby-expression body (extend-env ids args env))))))