#lang racket

(provide constraints operators apply-operator)

(define constraints '(noop positional alignment grouping))
(define operators '(== <= >= < > and or))

(define (apply-operator op lvalue rvalue)
  (match op
    ['== (= lvalue rvalue)]))

;
;#lang rosette/safe
;(define (make-variable)
;  (define-symbolic* v integer?)
;  (assert (>= v 0))
;  (assert (<  v (length constraints)))
;  v)
;(define (make-variables n)  
;  (append (list (make-variable)) (if (> n 1) (make-variables (- n 1)) '())))
