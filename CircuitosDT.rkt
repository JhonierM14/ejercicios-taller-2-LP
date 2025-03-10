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

(define valid-type?
  (lambda (type)
    (or (eqv? type 'and)
        (eqv? type 'or)
        (eqv? type 'not)
        (eqv? type 'xor)
    )
  )
)

(define-datatype type type?
  (gate-type (tipo valid-type?))
)

(define-datatype gate_id gate_id?
  (gate-id (id symbol?))
)
