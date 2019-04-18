#lang eopl
(require racket/string)
(require 2htdp/batch-io) ; Temporal para evaluación automática
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
(define lexical-spec
  '(
    (white-sp (whitespace) skip) 
    (comment ("#" (arbno (not #\newline))) skip) 
    (identifier ((arbno "@") letter (arbno (or letter digit "_" "?"))) symbol) 
    (number (digit (arbno digit)) number) 
    (number ("-" digit (arbno digit)) number) 
    (text ("\"" 
           (or letter whitespace digit) 
           (arbno (or letter digit whitespace ":" "?" "=" "'") ) "\"" ) 
          string) 
  )
)

;Especificación Sintáctica (gramática)
(define grammar-spec
  '(
    (ruby-program ("ruby" exp-batch "end") a-program)
    (exp-batch (expression (arbno expression)) a-batch)
    (expression (simple-exp) a-simple-exp)
    (expression ("declare" identifier (arbno "," identifier) ";") declare-exp)
    (expression ("puts" (separated-list comp-value ",") ";") puts-exp)
    (expression ("if" comp-value (arbno "then") exp-batch (arbno "elsif" comp-value (arbno "then") exp-batch)
                      (arbno "else" exp-batch) "end") if-exp)
    (expression ("unless" comp-value (arbno "then") exp-batch
                          (arbno "else" exp-batch) "end") unless-exp)
    (expression ("while" comp-value (arbno "do") exp-batch "end") while-exp)
    (expression ("until" comp-value (arbno "do") exp-batch "end") until-exp)
    (expression ("for" identifier "in" comp-value (arbno "do") exp-batch "end") for-exp)
    (expression ("def" identifier "(" (separated-list identifier ",") ")" exp-batch "end") function-exp)
    (expression ("return" comp-value ";") return-exp)

    (simple-exp (simple-value complement ";") val-exp)

    (complement ("=" comp-value calls) assign)
    (complement (assign-op comp-value calls) assign-and)
    (complement (calls) comp-calls)

    (calls ((arbno call)) some-calls)
    (call (arguments) arguments-call)
    ;; (call ("." identifier arguments) a-method-call) ;; Parte 2: Ruby con Objetos
    
    (arguments ("(" (separated-list comp-value ",") ")") some-arguments)
    (arguments ("[" comp-value (arbno "," comp-value) "]") arr-arguments)

    (comp-value (value) a-value)
    (comp-value (un-op comp-value) unop-value)
     
    (value (simple-value) a-s-val)
    (value ("(" comp-value val-compl ")") compl-val)
    (val-compl (calls) val-call)
    (val-compl (bin-op comp-value) binop-val)

    (simple-value (identifier) id-val)
    (simple-value (number) int-val)
    (simple-value (text) str-val) ;; recordar hacer string-trim cuando se evalue
    (simple-value ("true") true-val)
    (simple-value ("false") false-val)
    (simple-value ("nil") nil-val)
    (simple-value ("["(separated-list comp-value ",")"]") arr-val)

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
    (bin-op ("==") equal)
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
    (un-op ("not") not-op)
    (un-op ("!") not-op)
     ;;##############################################
     ;; Parte 2: Ruby con objetos
     ;(class-decl ("class" identifier
     ;                     (arbno "<" identifier)
     ;                     "attr" (separated-list ":" identifier ",") ";"
     ;                     (arbno method-decl) "end") a-class-decl)

     ;(method-decl ("def" identifier "(" (separated-list identifier ",") ")"
     ;             exp-batch                  
     ;             "end") a-method-decl)

  )
)

;Tipos de datos para la sintaxis abstracta de la gramática
(sllgen:make-define-datatypes lexical-spec grammar-spec)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexical-spec grammar-spec)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser lexical-spec grammar-spec))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner lexical-spec grammar-spec))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )
(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program pgm)) 
    (sllgen:make-stream-parser 
      lexical-spec
      grammar-spec)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define (eval-program pgm)
  (cases ruby-program pgm
    (a-program (a-batch) 
      (eval-exp-batch a-batch (empty-env))
      (eopl:pretty-print '=>nil)
    )
  )
)

;eval-exp-batch: Evalúa la última expresión
(define (eval-exp-batch batch env)
  (cases exp-batch batch
    (a-batch (exp exps) 
      (eval-expressions exp exps env)
    )
  )
)
;(eval-simple-expression exp env)
;eval-expressions: Evalúa la expresión en el ambiente de entrada
(define (eval-expressions exp exps env)
    (cases expression exp 

      (a-simple-exp (exp) 
        (eval-simple-expression exp env)
        (eval-next-exps exps env)
      )

      (declare-exp (id idss) 
        (let ((args (build-a-list (+ (length idss) 1) 'nil)))
          (eval-next-exps exps (extend-env (cons id idss) args env))
        )
      )

      (puts-exp (vals) 
        (map 
          (lambda (x) 
            (eopl:pretty-print (eval-comp-value x env))
          ) 
          vals
        )
        (eval-next-exps exps env)
      )

      (if-exp (test-exp true-exp test-exps2 true-exps2 false-exp)
        (if (eval-comp-value test-exp env)
          (begin 
            (eval-exp-batch true-exp env)
            (eval-next-exps exps env)
          )
          (begin
            (eval-elif test-exps2 true-exps2 false-exp env)
            (eval-next-exps exps env)
          )
        )
      )
      
      (unless-exp (test-exp exp else-exp)
        (if (eval-comp-value test-exp env)
          (if (null? else-exp)
            (eval-next-exps exps env)
            (begin
              (eval-exp-batch (car else-exp) env)
              (eval-next-exps exps env)
            )
          )   
          (begin
            (eval-exp-batch exp env)
            (eval-next-exps exps env)
          )
        )
      )
      
      (while-exp (test-exp exp)
        (let loop ()
          (when (eval-comp-value test-exp env)
            (eval-exp-batch exp env)
            (loop)
          )
        )
        (eval-next-exps exps env)
      )

      (until-exp (test-exp exp)
                 
        (let loop ()
          (when (not (eval-comp-value test-exp env))
            (eval-exp-batch exp env)
            (loop)
          )
        )
        (eval-next-exps exps env)
      )
      

      (for-exp (id range-exp exp)
        (let ((m_env (extend-env (list id) (list 'nil) env)))    
          (for-each
            (lambda (val)
              (apply-set-ref id val m_env)
              (eval-exp-batch exp m_env)
            )
            (eval-comp-value range-exp env)
          )
          (eval-next-exps exps env)
        )
      )

      (function-exp (id ids exp)
        (eval-next-exps exps (a-recursive-env id ids exp env))
      )

      (return-exp (val) 
        (eval-comp-value val env)
      ) 
    )
)

;eval-next-exps: Evalúa las siguientes expresiones
(define eval-next-exps
  (lambda (exps env)
    (cond 
      [(not (null? exps)) (eval-expressions (car exps) (cdr exps) env)]
    )
  )
)

;apply-set-ref: asigna un valor a un id
(define apply-set-ref
  (lambda (id value env)
    (setref! (apply-env-ref env id) value)))

; eval-elif: Función que retorna el valor del elsif evaluado si es true, si ninguno es true
;retorna el valor del else. Si no hay else, retorna =>'nil
(define eval-elif
  (lambda (test-exps true-exps false-exp env)
    (cond
      ((and (null? test-exps) (not (null? false-exp))) (eval-exp-batch (car false-exp) env))
      ((and (null? test-exps) (null? false-exp)) #f)
      ((eval-comp-value (car test-exps) env) (eval-exp-batch (car true-exps) env))
      (else (eval-elif (cdr test-exps) (cdr true-exps) false-exp env)))))

;Evaluaciones Complementarias
;****************************
;eval-simple-expression:evalua una expresión simple dentro de una expresión
(define eval-simple-expression
  (lambda (s-exp env)
    (cases simple-exp s-exp
      (val-exp (s-val comp) 
        (eval-complement s-val comp env)
      )
    )
  )
)

;eval-complement:evalua una complemento dentro de una expresión:
(define eval-complement
  (lambda (s-val compl env)
    (cases complement compl
      (assign (c-val calls) 
        (apply-set-ref
               (get-id s-val)
               (apply-call-list
                (eval-comp-value c-val env) calls env)
               env)
      )
      (assign-and (as-op c-val calls)
                  (apply-set-ref
                   (get-id s-val)
                   (apply-assign-op as-op
                                    (eval-simple-value s-val env)
                                    (apply-call-list
                                     (eval-comp-value c-val env) calls env))
                   env))
      (comp-calls (calls) (apply-call-list s-val calls env))
      )
    ))

;apply-assign-op:
(define apply-assign-op
  (lambda (as-op var-val a-val)
    (cases assign-op as-op
      (add-eq () (sum var-val a-val))
      (diff-eq () (- var-val a-val))
      (mult-eq () (multi var-val a-val))
      (div-eq () (divi var-val a-val))
      (pow-eq () (expt var-val a-val))
      )))

;eval-comp-value:
(define eval-comp-value
  (lambda (c-val env)
    (cases comp-value c-val
      (a-value (val) (eval-value val env))
      (unop-value (un-op comp-val) 
        (let ((exp (eval-comp-value comp-val env)))
          (apply-un-op un-op exp)
        )
      )
    )
  )
)

;eval-value:
(define eval-value
  (lambda (a-val env)
    (cases value a-val
      (a-s-val (val) (eval-simple-value val env))
      (compl-val (c-val val-comp)
        (eval-val-compl (eval-comp-value c-val env) val-comp env)
      )
    )
  )
)

;eval-val-compl: 
(define eval-val-compl
  (lambda (a-val a-v-compl env)
    (cases val-compl a-v-compl
      (val-call (calls) 
        (apply-call-list a-val calls env)
      )
      (binop-val (bin-op c-val) 
        (apply-bin-op bin-op a-val (eval-comp-value c-val env))
      )
    )
  )
)

;eval-simple-value:
(define eval-simple-value
  (lambda (s-val env)
    (cases simple-value s-val
      (id-val (id) (apply-env env id))
      (int-val (num) num)
      (str-val (val) (substring val 1 (- (string-length val) 1)) )
      (true-val () #t)
      (false-val () #f)
      (nil-val () '=>nil)
      (arr-val (c-vals) (map (lambda (x) (eopl:pretty-print(eval-comp-value x env))) c-vals)) ;probando
    )
  )
)

;apply-un-op:
(define apply-un-op
  (lambda (op exp)
    (cases un-op op
      (not-op () (not exp))
      )))

;apply-call-list:
(define apply-call-list
  (lambda (value c-list env)
    (cases calls c-list
      (some-calls (calls) 
        (if (null? calls)
          value
          (apply-call value (car calls) env)  ;;;; BETA 
        )
      )
    )
  )
)

;apply-call:
(define apply-call
  (lambda (value a-call env)
    (cases call a-call
;     (method-call (id args) id)
      (arguments-call (args) 
        (apply-arguments value args env)
      )
    )
  )
)

;apply-arguments:  
(define apply-arguments
  (lambda (a-val args env)
    (cases arguments args
      (some-arguments (vals) 
        (let ([args (map (lambda (x)(eval-comp-value x env) ) vals) ])
          (if (procval? a-val)
            (apply-procedure a-val args env)
            (let ((proc (eval-simple-value a-val env)))
              (if (procval? proc)
                (apply-procedure proc args env)
                (eopl:error 'eval-expression "Attempt to apply non-procedure ~s" proc)
              )
            )
          )
        )
      )
      (arr-arguments (val vals) a-val)
    )
  )
)

;apply-bin-op:
(define apply-bin-op
  (lambda (op arg1 arg2)
    (cases bin-op op
      (add () 
        (sum arg1 arg2)
      )
      (diff () (- arg1 arg2))
      (mult () 
        (multi arg1 arg2)
      )
      (div () 
        (divi arg1 arg2)
      )
      (mod () (modulo arg1 arg2))
      (pow () (expt arg1 arg2))
      (great () (> arg1 arg2))
      (great-eq () (>= arg1 arg2))
      (less () (< arg1 arg2))
      (less-eq () (<= arg1 arg2))
      (equal () (equal? arg1 arg2))
      (not-equal () (not(equal? arg1 arg2)))
      (and-op () (and arg1 arg2))
      (or-op () (or arg1 arg2))
      (in-range () (if (and (number? arg1) (number? arg2)) (eval-range (inclusive arg1 arg2 1)) 'error))
      (ex-range () (if (and (number? arg1) (number? arg2)) (eval-range (exclusive arg1 arg2 1)) 'error))
      (st-range () (let ((list (eval-comp-value arg1 empty-env)))
                     (eval-range (inclusive (car list) (list-ref list (length list)) arg2))))
      )))

;*******************************************************************************************
;Referencias
(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

;*******************************************************************************************
;Datatypes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec  vector?)
   (env environment?)))

(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body exp-batch?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args env)
    (cases procval proc
      (closure (ids body env)
        (eval-exp-batch body (extend-env ids args env))
      )
    )
  )
)

(define scheme-value? (lambda (v) #t))

;*******************************************************************************************
;Ambientes

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 

(define (init-env) (empty-env))

;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env)))
            (iota len) idss bodies)
          env)))))

;Ambiente recursivo para un solo procedimiento
(define (a-recursive-env a-proc-name ids body env)
  (let ((vec (make-vector 1)))
    (let ((env (extended-env-record (list a-proc-name) vec env)))
      (vector-set! vec 0 (closure ids body env))
      env
    )
  )
)

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'Error "undefined local variable or method ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))

;*******************************************************************************************
; Funciones auxiliares
;get-id: Retorna el id de un s-val
(define get-id
  (lambda (s-val)
    (cases simple-value s-val
      (id-val (id) id)
      (else (eopl:error 'Error "Expected id, given ~s" s-val)))))

;Función para aplicar eval-expression a cada elemento de una lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-exp-batch rand env)))

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente
(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

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

(define sum
  (lambda (arg1 arg2) 
    (cond
      [(and (number? arg1) (number? arg2)) 
        (+ arg1 arg2)
      ]
      [(and (string? arg1) (string? arg2)) 
        (string-append arg1 arg2)
      ]
      [(and (list? arg1) (list? arg2)) 
        (append arg1 arg2)
      ]
      [(and (string? arg1) (number? arg2)) 
        (eopl:error 'Error "No implicit conversion of Integer into String")
      ]
      [(and (number? arg1) (string? arg2)) 
        (eopl:error 'Error "String can't be coerced into integer")
      ]
    )
  )
)

(define multi
  (lambda (arg1 arg2)
    (cond
      [(and (number? arg1) (number? arg2)) 
        (* arg1 arg2)
      ]
      [(and (string? arg1) (number? arg2)) 
        (let lp ([n (- arg2 1)])
          (if (zero? n)
            arg1
            (string-append arg1 (lp (- n 1)))
          )
        ) 
      ]
      [(and (number? arg1) (string? arg2)) 
        (let lp ([n (- arg1 1)])
          (if (zero? n)
            arg2
            (string-append arg2 (lp (- n 1)))
          )
        ) 
      ]
      [(and (list? arg1) (number? arg2)) 
        (let lp ([n (- arg2 1)])
          (if (zero? n)
            arg1
            (append arg1 (lp (- n 1)))
          )
        ) 
      ]
      [(and (number? arg1) (list? arg2)) 
        (let lp ([n (- arg1 1)])
          (if (zero? n)
            arg2
            (append arg2 (lp (- n 1)))
          )
        ) 
      ]
    )
  )
)

(define divi
  (lambda (arg1 arg2)
    (cond
      [(and (number? arg1) (number? arg2)) 
        (/ arg1 arg2)
      ]
      [(and (string? arg1) (number? arg2)) 
        (eopl:error 'Error "No implicit conversion of Integer into String")
      ]
      [(and (number? arg1) (string? arg2)) 
        (eopl:error 'Error "String can't be coerced into integer")
      ]
    )
  )
)

;*******************************************************************************************
;;;Rangos
(define-datatype range range?
  (inclusive (start number?) (end number?) (step number?))
  (exclusive (start number?) (end number?) (step number?))
  )

(define (eval-range a-range)
  (cases range a-range
    (inclusive (start end step) (iota-range start end step))
    (exclusive (start end step) (iota-range start (- end 1) step))
    )
  )

;;Función que retorna una lista dado un inicio, un final, y un incremento 
(define iota-range
  (lambda (start end step)
    (cond [(or
            (and (< start end) (> 0 step))
            (and (> start end) (< 0 step)))
           (eopl:error 'Step "bad step")]
          [else
           (let loop ((next start))
             (if (= 0 (abs (- next end)))
                 (list next)
                 (cons next (loop (+ next step)))))]
          )))

;build-a-list: Construye una lista de n elementos con valor = val
(define build-a-list
  (lambda (n val)
    (cond
      ((equal? n 0) empty)
      (else (cons val (build-a-list (- n 1) val))))))

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))

(
    (lambda (pgm) (eval-program  pgm)) 
    (scan&parse  (string-append "ruby " (read-file "input.rb") " end"))
)
