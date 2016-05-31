#lang rosette

(provide get-predicates)

(define (get-predicates program)
  (map
   (lambda (set) (match set
                   [`(def ,name ,predicate ,statements ...) predicate]))
   program))

