;;-----------------------------------------
;; Jhonier Mendez Bravo 202372226
;; David Santiago Guerrero Delgado 202324594
;; Juan Pablo Robayo Maestre 202156743
;;-----------------------------------------

#lang eopl

#|
para la representacion basada en listas, construya una funcion PARSEBNF
donde dada una lista con la representacion concreta de un circuito,
construya el arbol de sintaxis abstracta basado en datatypes.

para la representacion basada en datatypes, construya 
una funcion UNPARSEBNF donde dado un arbol de 
sintaxis abstracta de un circuito, entregue la 
representacion concreta basada en listas.
|#

;; INTERFAZ

;; ################################################
;;
;;        REPPRESENTACION BASADA EN LISTAS
;;
;; ################################################

;; datatype de circuito
(define-datatype circuit circuit?
  (a-circuit (gate_list (list-of gate?)))
  )

;;datatype de gate
(define-datatype gate gate?
  (a-gate (id symbol?) (type gate-type?) (inputs (list-of input?))))

;;datatype de type
(define-datatype gate-type gate-type?
  (not-type)
  (and-type)
  (or-type)
  (xor-type))

;;datatype de input
(define-datatype input input?
  (bool-input (val boolean?))
  (ref-input (id symbol?)))

;; --------------------- PARSEBNF ---------------------

;; expr := representacion concreta del circuito

(define (PARSEBNF expr)
  (cond
    [(and (list? expr) (= (length expr) 2) (eq? (car expr) 'circuit))
     (a-circuit (parse-gates (cadr expr)))]
    [else (eopl:error "NO SE INGRESO UNA LISTA VALIDA")]))

;; Se verfica que una lista sea gate_list
(define (parse-gates gates)
  (cond
    [(and (list? gates) (> (length gates) 1) (eq? (car gates) 'gate_list))
     (parse-gate-list (cdr gates))]
    [else (eopl:error "NO SE INGRESO UNA LISTA VALIDA")]))

;; Condicion de parada, y llamada a parse-gate para formaterar el gate
(define (parse-gate-list gate-list)
  (cond
    [(null? gate-list) '()]
    [else (cons (parse-gate (car gate-list)) (parse-gate-list (cdr gate-list)))]))

;; Se verifica que la lista sea un gate valido, y se establece la estructura
(define (parse-gate gate)
  (cond
    [(and (list? gate) (= (length gate) 4) (eq? (car gate) 'gate))
     (a-gate (cadr gate) (parse-type (caddr gate)) (parse-inputs (cadddr gate)))]
    [else (eopl:error "Formato de compuerta inválido")]))

;; Retorna una lista formateada para el type
(define (parse-type type)
  (cond
    [(equal? type '(type not)) (not-type)]
    [(equal? type '(type and)) (and-type)]
    [(equal? type '(type or)) (or-type)]
    [(equal? type '(type xor)) (xor-type)]
    [else (eopl:error "Tipo de compuerta inválido")]))

;; Crea y retorna una lista de inputs formateados
(define (parse-inputs inputs)
  (if (null? (cdr inputs))
      '()
      (cons (parse-input (cadr inputs)) (parse-inputs (cons 'input-list (cddr inputs))))))

;; Retorna la representacion de un input para el arbol de sintaxis abstracta
(define (parse-input input)
  (if (boolean? input)
      (bool-input input)
      (ref-input input)))

;; --------------------- Ejemplos ---------------------

(PARSEBNF
 '(circuit
       (gate_list
           (gate G1 (type not) (input-list #t)))))

(PARSEBNF
 '(circuit
   (gate_list
    (gate G1 (type not) (input_list A)))))

(PARSEBNF
 '(circuit
   (gate_list
    (gate G1 (type and) (input_list A B )))))

(PARSEBNF '(circuit (gate_list
                     (gate G1 (type or) (input-list A B))
                     (gate G2 (type and) (input-list A B))
                     (gate G3 (type not) (input-list G2))
                     (gate G4 (type and) (input-list G1 G3)))))

;; ------------------------------------------------------------------------------

(define UNPARSEBNF
  (lambda (expresion)
    (cases circuit expresion
      (primerElemento (name) name)
      (segundoElementoGateList (name body) ;lambda crea recursivamente los gate_list
        (list 'gate_list (list name)
              (UNPARSEBNF body)))
      (tercerElementoGate (rator rand)
         (list (UNPARSEBNF rator) (UNPARSEBNF rand))))))

;;Pruebas
(UNPARSEBNF
(PARSEBNF
 '(circuit
       (gate-list
           (gate G1 (type not) (input-list #t)))))
)


