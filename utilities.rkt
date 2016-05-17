#lang rosette/safe

(provide make-variable make-variables)

(define (make-variable)
  (define-symbolic* v integer?)
  (assert (>= v 0))
  (assert (<  v (length constraints)))
  v)
(define (make-variables n)  
  (append (list (make-variable)) (if (> n 1) (make-variables (- n 1)) '())))
