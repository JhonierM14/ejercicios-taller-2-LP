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
|#

(define circuit 
  (lambda (gate-list) 
    (list 'circuit gate-list)))

(define gate-list 
  (lambda (gate gate-list)
    (list 'gate_list gate gate-list)))

(define gate 
  (lambda (gate-id type input-list)
    (list 'gate gate-id type input-list)))

(define input-list 
  (lambda input
    (if (= (length input) 1)
        (list 'input_list (car input))  
        (list 'input_list (car input) (cadr input)))))

(define type
  (lambda (value)
    (list 'type value)))

;; --------------------- Extractores ---------------------

(define circuit->gate-list 
  (lambda (circuit) 
    (cadr circuit)))

(define gate-list->first 
  (lambda (gate-list) 
    (car gate-list)))

(define gate-list->rest 
  (lambda (gate-list) 
    (cadr gate-list)))

(define gate->gate-id 
  (lambda (gate) 
    (cadr gate)))

(define gate->type 
  (lambda (gate) 
    (caddr gate)))

(define gate->input-list 
  (lambda (gate) 
    (cadddr gate)))

(define input-list->first 
  (lambda (input-list) 
    (car input-list)))

(define input-list->rest 
  (lambda (input-list) 
    (cadr input-list)))

;; --------------------- Predicados ---------------------

(define circuit? 
  (lambda (circuit)
    (and (list? circuit) (eq? (car circuit) 'circuit))))

(define gate-list? 
  (lambda (gate-list)
    (list? gate-list)))

(define gate? 
  (lambda (gate)
    (and (list? gate) (eq? (car gate) 'gate))))

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


