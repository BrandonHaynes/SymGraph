#lang racket

(provide constraints unary-operators binary-operators apply-binary-operator apply-unary-operator)

(define constraints '(noop positional alignment grouping))
(define binary-operators '(== <= >= < > and or))
(define unary-operators '(not))

(define (apply-binary-operator op lvalue rvalue)
  (match op
    ['==  (=   lvalue rvalue)]
    ['>=  (>=  lvalue rvalue)]
    ['<=  (<=  lvalue rvalue)]
    ['<   (<   lvalue rvalue)]
    ['>   (>   lvalue rvalue)]
    ['and (and lvalue rvalue)]
    ['or  (or  lvalue rvalue)]))

(define (apply-unary-operator op value)
  (match op
    ['not (not value)]))

;
;#lang rosette/safe
;(define (make-variable)
;  (define-symbolic* v integer?)
;  (assert (>= v 0))
;  (assert (<  v (length constraints)))
;  v)
;(define (make-variables n)  
;  (append (list (make-variable)) (if (> n 1) (make-variables (- n 1)) '())))
