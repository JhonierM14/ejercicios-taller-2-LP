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
(define-datatype circuito circuito?
  (a-circuit (gate_list (list-of gates?)))
  )

;;datatype de gate
(define-datatype gates gates?
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
; debe retornar el arbol de sintaxis abstracta

(PARSEBNF
 '(circuit
       (gate_list
           (gate G1 (type not) (input_list #t)))))

(PARSEBNF
 '(circuit
   (gate_list
    (gate G1 (type not) (input_list A)))))

(PARSEBNF
 '(circuit
   (gate_list
    (gate G1 (type and) (input_list A B )))))

(PARSEBNF '(circuit (gate_list
                     (gate G1 (type or) (input_list A B))
                     (gate G2 (type and) (input_list A B))
                     (gate G3 (type not) (input_list G2))
                     (gate G4 (type and) (input_list G1 G3)))))


;; ################################################
;;
;;        REPPRESENTACION BASADA EN DATATYPES
;;
;; ################################################

; LISTA CON ('CIRCUIT (LIST GATES-LIST))
(define (UNPARSEBNF arbol)
  (cases circuito arbol
      (a-circuit (gate_list)
                 (list 'circuit (cons 'gate_list (unparsebnf-gate-list gate_list))))))

; CREA LA LISTA DE GATE_LIST
(define (unparsebnf-gate-list gate-list)
  (if (null? gate-list)
      '()
      (cons (unparser-gate (car gate-list))
            (unparsebnf-gate-list (cdr gate-list)))))

; CREA LOS GATES
(define (unparsebnf-gate-list-gate gate-list)
  (if (null? gate-list)
      '()
      (cons (unparser-gate (car) (unparsebnf-gate-list-gate (cdr gate-list))))))

; Procesa un gate individualmente
(define (unparser-gate gate)
  (cases gates gate
    (a-gate (id type inputs)
     (list 'gate id (unparse-type type) (cons 'input_list (unparse-inputs inputs))))))

; Retorna el type y su estado
(define (unparse-type type)
  (cases gate-type type
    (not-type () (list 'type 'not))
    (and-type () (list 'type 'and))
    (or-type () (list 'type 'or))
    (xor-type () (list 'type 'xor))))

; Crea la lista de inputs
(define (unparse-inputs inputs)
  (if (null? inputs) 
      '()
      (cons (unparse-input (car inputs)) (unparse-inputs (cdr inputs)))))

; Formatea un input
(define (unparse-input el-input)
  (cases input el-input
    (bool-input (val) val)
    (ref-input (val) val)
    ))
  
; ------------------ Ejemplos------------------------
; deben retornar la lista de circuito

(UNPARSEBNF (PARSEBNF
 '(circuit
       (gate_list
           (gate G1 (type not) (input-list #t))))))

(UNPARSEBNF (PARSEBNF
 '(circuit
   (gate_list
    (gate G1 (type not) (input_list A))))))

(UNPARSEBNF (PARSEBNF
 '(circuit
   (gate_list
    (gate G1 (type and) (input_list A B ))))))

(UNPARSEBNF (PARSEBNF '(circuit (gate_list
                     (gate G1 (type or) (input_list A B))
                     (gate G2 (type and) (input_list A B))
                     (gate G3 (type not) (input_list G2))
                     (gate G4 (type and) (input_list G1 G3))))))
