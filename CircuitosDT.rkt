;;-----------------------------------------
;; Jhonier Mendez Bravo 202372226
;; David Santiago Guerrero Delgado 202324594
;; Juan Pablo Robayo Maestre 202156743
;;-----------------------------------------

#lang eopl

#|
(20 Pts) proponga una implementacion basada en datatypes 
(hay varias formas como puede representar la gramatica, 
en especial dependiendo del manejo que le de a las listas 
gate list e input list). Incluya ejemplos donde
se evidencie su utilizacion y la creacion de por lo menos 
los 4 ejemplos de este taller. Revise los ejemplos de 
sintaxis abstracta de la siguiente seccion que le podran 
servir como guia.
|#

;;-----------------------------------GRAMATICA-----------------------------------

;; <circuito> ::=  (<gate_list>)
                   ; circuit (gate-list)

;; <gate_list> ::= <empty>
                 ; empty_gate_list
;;             ::= <gate> <gate_list>
                ; gate_list (gate gate-list)

;; <gate> ::= (<gate_id> <type> <input_list>)
            ; gate (gate-id tipo input-list)

;; <gate_id> ::= <symbol>
               ; gate_id (id)

;; <type> ::= and
            ; type_and
;;        ::= or
            ; type_or
;;        ::= not
            ; type_not
;;        ::= xor
             ; type_xor

;; <input_list> ::= <empty>
                  ; empty-input-list
;;              ::= <bool> <input_list>
                  ; input_list_A (bool input-lst)
;;              ::= <gate_ref> <input_list>
                  ; input_list_B (gate-ref input-lst)

;; <gate_ref> ::= symbol
                ; gate_ref (id)

;;-----------------------------------DATATYPES-----------------------------------

;; Definición del tipo de dato `gate-type`, que representa los tipos de compuertas lógicas que puede haber en el circuito.
;; Cada tipo corresponde a una compuerta lógica: AND, OR, NOT y XOR.
(define-datatype gate-type gate-type?
  (type_and)
  (type_or)
  (type_not)
  (type_xor)
)

;; Definición del tipo de dato `gate-id`, que representa el identificador (nombre) de una compuerta lógica.
;; Se usa para referenciar y distinguir cada compuerta dentro del circuito.
(define-datatype gate-id gate-id?
  (gate_id (id symbol?))
)

;; Definición del tipo `gate-ref`, que permite referenciar otra compuerta lógica ya definida.
;; Se utiliza cuando una compuerta toma como entrada la salida de otra compuerta.
(define-datatype gate-ref gate-ref?
  (gate_ref (id symbol?))
)

;; Definición del tipo `gate-list`, que representa una lista de compuertas lógicas en el circuito.
;; Puede ser una lista vacía (empty_gate_list) o una lista compuesta por una compuerta y el resto de la lista.
(define-datatype gate-list gate-list?
  (empty_gate_list)
  (gate_list (gate gte?) (gate-list gate-list?))
)

;; Definición del tipo `gte`, que representa una compuerta lógica individual dentro del circuito.
;; Cada compuerta tiene un identificador, un tipo y una lista de entradas.
(define-datatype gte gte?
  (gate (gate-id gate-id?) (tipo gate-type?) (input-list input-list?))
)

;; Definición del tipo `input-list`, que representa las entradas de una compuerta lógica.
;; Las entradas pueden ser valores booleanos directos con otra input-list o referencias a otras compuertas otra input-list.
(define-datatype input-list input-list?
  (empty-input-list)
  (input_list_A (bool boolean?) (input-lst input-list?))
  (input_list_B (gate-ref gate-ref?) (input-lst input-list?))
)

;; Definición del tipo `crct`, que representa el circuito completo.
;; Un circuito está compuesto por una lista de compuertas.
(define-datatype crct crct?
  (circuit (gate-list gate-list?))
)

;;-----------------------------------EJEMPLOS-----------------------------------

#|
(circuit
   (gate_list
    (gate (gate_id 'G1) (type_not) (input_list_A #t (empty-input-list)))
    (empty_gate_list)))
|#

#|
(circuit
   (gate_list
    (gate (gate_id 'G1) (type_not) (input_list_B (gate_ref 'A) (empty-input-list)))
    (empty_gate_list)))
|#

#|
(circuit
   (gate_list
    (gate (gate_id 'G1) (type_and) (input_list_B (gate_ref 'A) (input_list_B (gate_ref 'B) (empty-input-list))))
    (empty_gate_list)))
|#

#|
(circuit
   (gate_list
    (gate (gate_id 'G1) (type_or) (input_list_B (gate_ref 'A) (input_list_B (gate_ref 'B) (empty-input-list))))
    (gate_list
      (gate (gate_id 'G2) (type_and) (input_list_B (gate_ref 'A) (input_list_B (gate_ref 'B) (empty-input-list))))
       (gate_list (gate (gate_id 'G3) (type_not) (input_list_B (gate_ref 'G2) (empty-input-list)))
         (gate_list (gate (gate_id 'G4) (type_and) (input_list_B (gate_ref 'G1) (input_list_B (gate_ref 'G3) (empty-input-list)))) (empty_gate_list))))))
   
|#
