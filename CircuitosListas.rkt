;;-----------------------------------------
;; Jhonier Mendez Bravo 202372226
;; David Santiago Guerrero Delgado 202324594
;; Juan Pablo Robayo Maestre 202156743
;;-----------------------------------------

#lang eopl

#|
(20 Pts) proponga
una implementacion de la gram ́tica basada en listas: Esta 
implementacion deberia contener los respectivos constructores (circuit,
gate list, gate, type, input list). gate id y gate ref son simbolos y
no requeren un constructor. La implementacion tambien debe contener 
extractores (circuit- > gate list, gate list- > first, gate list- >
rest, gate- > gate id, gate- > type, gate- > input list, input list- >
first, input list- > rest). Incluya ejemplos donde se evidencie
su utilizacion y la creacion de por lo menos los 4 ejemplos de este
taller.


Gramatica

<circuit> ::= '(circuit <gate-list>)
<gate-list> ::= empty | <gate> <gate-list>
<gate> ::= '(gate <gate-id> <type> <input-list>)
<gate-id> ::= identificador único
<type> ::= and | or | not | xor
<input-list> ::= empty | <bool> <input-list> | <gate-ref> <input-list>
<gate-ref> ::= identificador de otra compuerta

|#

;; Construye un circuito con una lista de compuertas
(define circuit 
  (lambda (gate-list) 
    (list 'circuit gate-list)))

;; Crea una lista de compuertas, permitiendo anidamiento o estar vacía
(define gate-list 
  (lambda (gate gate-list)
    (list 'gate_list gate gate-list)))

;; Define una compuerta lógica con identificador, tipo y lista de entradas
(define gate 
  (lambda (gate-id type input-list)
    (list 'gate gate-id type input-list)))

;; Construye una lista de entradas para las compuertas, permitiendo 1 o 2 argumentos
(define input-list 
  (lambda input
    (if (= (length input) 1)
        (list 'input_list (car input))  
        (list 'input_list (car input) (cadr input)))))

;; Representa el tipo de una compuerta lógica
(define type
  (lambda (value)
    (list 'type value)))

;; --------------------- Extractores ---------------------

;; Obtiene la lista de compuertas de un circuito
(define circuit->gate-list 
  (lambda (circuit) 
    (cadr circuit)))

;; Extrae la primera compuerta de una lista de compuertas
(define gate-list->first 
  (lambda (gate-list) 
    (car gate-list)))

;; Extrae el resto de la lista de compuertas
(define gate-list->rest 
  (lambda (gate-list) 
    (cadr gate-list)))

;; Extrae el identificador de una compuerta
(define gate->gate-id 
  (lambda (gate) 
    (cadr gate)))

;; Extrae el tipo de una compuerta
(define gate->type 
  (lambda (gate) 
    (caddr gate)))

;; Obtiene la lista de entradas de una compuerta
(define gate->input-list 
  (lambda (gate) 
    (cadddr gate)))

;; Obtiene el primer elemento de la lista de entradas
(define input-list->first 
  (lambda (input-list) 
    (car input-list)))

;; Obtiene el resto de la lista de entradas
(define input-list->rest 
  (lambda (input-list) 
    (cadr input-list)))

;; --------------------- Predicados ---------------------

;; Verifica si un objeto es un circuito
(define circuit? 
  (lambda (circuit)
    (and (list? circuit) (eq? (car circuit) 'circuit))))

;; Verifica si un objeto es una lista de compuertas
(define gate-list? 
  (lambda (gate-list)
    (list? gate-list)))

;; Verifica si un objeto es una compuerta
(define gate? 
  (lambda (gate)
    (and (list? gate) (eq? (car gate) 'gate))))

;; Verifica si un objeto es una lista de entradas
(define input-list? 
  (lambda (input-list)
    (list? input-list)))

;; --------------------- Ejemplos ---------------------

(define gate1 (gate 'G1 (type 'not) (input-list 'A 'B)))
(define gate2 (gate 'G2 (type 'and) (input-list 'A 'B)))
(define gate3 (gate 'G3 (type 'or) (input-list 'C)))
(define gate4 (gate 'G4 (type 'xor) (input-list 'G2 'G3)))

(define circuit1 (circuit (gate-list gate1 gate3)))
(define circuit2 (circuit (gate-list gate2 gate1)))
(define circuit3 (circuit (gate-list gate3 gate4)))


