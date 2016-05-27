#lang rosette/safe

(provide make-variable make-variables get-assignments)

(define (make-variable)
  (define-symbolic* v integer?)
  v)

(define (make-variables n)  
  (append (list (make-variable)) (if (> n 1) (make-variables (- n 1)) '())))

(define (get-assignments solution)
  (model solution))
