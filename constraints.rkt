#lang rosette/safe

(require "state.rkt" "utilities.rkt" "graph.rkt" "variables.rkt")

(provide align-constraint position-constraint ordered-position-constraint)

(define (align-constraint state axis index pair)
  (define vconstraint (register-variable state `(,pair ,index constraint)))
  (assert (= vconstraint (list-index 'alignment constraints)))
  (define vaxis (register-variable state `(,pair ,index 'metadata 0)))
  (assert (= vaxis (list-index axis axes))))

(define (position-constraint state graph axis attribute index pair)
  (define vconstraint (register-variable state `(,pair ,index constraint)))
  (assert (= vconstraint (list-index 'positional constraints)))
  (define vaxis (register-variable state `(,pair ,index 'metadata 0)))
  (assert (= vaxis (list-index axis axes)))
  (define vorder (register-variable state `(,pair ,index 'metadata 1)))
  (assert (= vorder (- (get-attribute graph (car pair) attribute)
                               (get-attribute graph (cadr pair) attribute)))))

(define (ordered-position-constraint state axis order index pair)
  (define vconstraint (register-variable state `(,pair ,index constraint)))
  (assert (= vconstraint (list-index 'positional constraints)))
  (define vaxis (register-variable state `(,pair ,index 'metadata 0)))
  (assert (= vaxis (list-index axis axes)))
  (define vorder (register-variable state `(,pair ,index 'metadata 1)))
  (assert (= vorder (compare (car pair) (cadr pair) order))))

