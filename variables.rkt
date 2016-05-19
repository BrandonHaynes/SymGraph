#lang rosette

(provide make-variable make-variables)

(define (make-variable)
  (define-symbolic* v integer?)
  v)

(define (make-variables n)  
  (append (list (make-variable)) (if (> n 1) (make-variables (- n 1)) '())))
