#lang racket

(provide constraints unary-operators binary-operators axes
         apply-binary-operator
         apply-unary-operator
         list-index)

(define constraints '(noop positional alignment grouping))
(define binary-operators '(== <= >= < > and or))
(define unary-operators '(not))
(define axes '(x-axis y-axis))

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

(define (list-index element list [index 0])
  (cond
    [(empty? list) #f]
    [(equal? (car list) element) index]
    [else (list-index element (cdr list) (+ index 1))]))
