#lang eopl

(require 2htdp/batch-io)
(require "lenguaje.rkt")
(require "evaluador.rkt")

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)
(define scan&parse
	(sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter)
)

;El Analizador Léxico (Scanner)
(define just-scan
	(sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter)
)

(
    (lambda (pgm) (eval-program  pgm)) 
    (scan&parse  (read-file "input.rb"))
)