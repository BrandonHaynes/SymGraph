#lang racket

(provide constraints unary-operators binary-operators axes
         apply-binary-operator
         apply-unary-operator
         list-index enumerate
         replace)

(define constraints '(noop positional alignment grouping))
(define binary-operators '(== <= >= < > and or in))
(define unary-operators '(not min))
(define axes '(x-axis y-axis))

; TODO change to eval
(define (apply-binary-operator op lvalue rvalue)
  (match op
    ['==  (=   lvalue rvalue)]
    ['>=  (>=  lvalue rvalue)]
    ['<=  (<=  lvalue rvalue)]
    ['<   (<   lvalue rvalue)]
    ['>   (>   lvalue rvalue)]
    ['and (and lvalue rvalue)]
    ['or  (or  lvalue rvalue)]
    ['in  (member lvalue rvalue)]))

(define (apply-unary-operator op value)
  (match op
    ['not
     (not value)]
    ['min
     (if (equal? value '()) +nan.0 (argmin identity value))]))

;(define (apply-set-membership v set)
;  (match op
;    ['not (not value)]))

(define (list-index element list [index 0])
  (cond
    [(empty? list) #f]
    [(equal? (car list) element) index]
    [else (list-index element (cdr list) (+ index 1))]))

(define (enumerate list [index 0])
  (if (empty? list)
      '()
      (cons (cons index (car list)) (enumerate (cdr list) (+ index 1)))))

(define (replace value replacement list)
  (cond ((null? list)
          '())
        ((equal? (car list) value)
          (replace value replacement (cons replacement (cdr list))))
        (else (cons (car list) (replace value replacement (cdr list))))))
