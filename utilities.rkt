#lang rosette/safe

(provide constraints unary-operators binary-operators axes
         apply-binary-operator
         apply-unary-operator
         list-index enumerate
         replace
         compare)

(define constraints '(noop positional alignment grouping))
(define binary-operators '(= <= >= < > and or in))
(define unary-operators '(not min))
(define axes '(x-axis y-axis))

; TODO change to eval
(define (apply-binary-operator op lvalue rvalue)
  (cond
    [(equal? op '=) (= lvalue rvalue)]
    [(equal? op '>=) (>= lvalue rvalue)]
    [(equal? op '<=) (<= lvalue rvalue)]
    [(equal? op '<) (< lvalue rvalue)]
    [(equal? op '>) (> lvalue rvalue)]
    [(equal? op 'and) (and lvalue rvalue)]
    [(equal? op 'or) (or lvalue rvalue)]
    [(equal? op 'in) (member lvalue rvalue)]))

(define (apply-unary-operator op value)
  (cond
    [(equal? op 'not) (not value)]
    [(equal? op 'min) (if (equal? value '()) +nan.0 (argmin identity value))]))

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

(define (compare left right order)
  (define left-group (order-group left order))
  (define right-group (order-group right order))
  (cond
    [(equal? left-group right-group) 0]
    [null? left-group -1]
    [((get-comparator left) left-group right-group) -1]
    [else 1]))

(define (order-group value order)
  (printf "~a ~a\n" value order)
  (cond
    [(empty? order) null]
    [((get-comparator value) value (car (flatten order))) (car order)]
    [else (order-group value (cdr order))]))

(define (get-comparator value)
  (cond
    [(number? value) <]
    ;[(string? value) string<?]
    ;[(symbol? value) symbol<?]
    ))