;;-----------------------------------------
;; Jhonier Mendez Bravo 202372226
;; David Santiago Guerrero Delgado 202324594
;; Juan Pablo Robayo Maestre 202156743
;;-----------------------------------------

#lang eopl

(define valid-type?
  (lambda (type)
    (or (eqv? type 'and)
        (eqv? type 'or)
        (eqv? type 'not)
        (eqv? type 'xor)
    )
  )
)

(define-datatype gate-type gate-type?
  (type (tipo valid-type?))
)

(define-datatype gate-id gate-id?
  (gate_id (id symbol?))
)

(define-datatype gate-ref gate-ref?
  (gate_ref (id symbol?))
)

(define-datatype gate-list gate-list?
  (empty_gate_list)
  (gate_list (gate gte?) (gate-list gate-list?))
)

(define-datatype gte gte?
  (gate (gate-id symbol?) (tipo gate-type?) (input-list input-list?))
)

(define-datatype input-list input-list?
  (empty-input-list)
  (input_list_A (bool boolean?) (input-lst input-list?))
  (input_list_B (gate-ref gate-ref?) (input-lst input-list?))
)

(define-datatype crct crct?
  (circuit (gate-list gate-list?))
)

#|
(circuit
   (gate_list
    (gate 'G1 (type 'or) (input_list_B (gate_ref 'A) (input_list_B (gate_ref 'B) (empty-input-list))))
    (gate_list
      (gate 'G2 (type 'and) (input_list_B (gate_ref 'A) (input_list_B (gate_ref 'B) (empty-input-list))))
       (gate_list (gate 'G3 (type 'not) (input_list_B (gate_ref 'G2) (empty-input-list)))
         (gate_list (gate 'G4 (type 'and) (input_list_B (gate_ref 'G1) (input_list_B (gate_ref 'G3) (empty-input-list)))) (empty_gate_list))))))
   
|#
