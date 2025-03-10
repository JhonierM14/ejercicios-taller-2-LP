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
  (gate (gate-id gate-id?) (tipo gate-type?) (input-list input-list?))
)

(define-datatype input-list input-list?
  (empty-input-list)
  (input_list (bool boolean?) (input-lst input-list?) (gate-ref gate-ref?) (inpt-lst input-list?))
)
