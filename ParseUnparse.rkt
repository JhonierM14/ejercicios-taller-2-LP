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

;;---------------------------------
;;
;;       FALTA DATATYPE
;;
;;---------------------------------

(define isgate_list?
  (lambda (L)
    (if (null? L) #t (if (eqv? (caar L) 'gate) (isgate_list? (cdr L)) #f) )
    ))

(define isgate?
  (lambda (L)
    (if (eqv? (car L) 'gate) #t #f)
    ))

(define PARSEBNF
  (lambda (circuit)
    (cond
      [(null? circuit) circuit]
      [(symbol? circuit) (constuirSimbolo circuit)]
      [(eqv? (car circuit) 'circuit) (construirCircuito PARSEBNF(cdr circuit))]
      [(isgate_list? circuit) (construirGateList (car circuit) PARSEBNF(cdr circuit))]
      [(isgate? circuit) (construirGate PARSEBNF(circuit))]
      [else (eopl:error "Invalid concrete syntax" circuit)]
      )))

;;Pruebas
(PARSEBNF
 '(circuit
       (gate-list
           (gate G1 (type not) (input-list #t)))))

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


