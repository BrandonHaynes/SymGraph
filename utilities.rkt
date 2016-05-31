#lang rosette

(provide constraints operators axes
         apply-operator
         list-index enumerate
         replace
         compare)

(define constraints '(noop positional alignment grouping))
(define operators '(= <= >= < > and or in not min))
(define binary-operators '(= <= >= < > and or in))
(define unary-operators '(not min))
(define axes '(x-axis y-axis))

(define-namespace-anchor anchor)
(define namespace (namespace-anchor->namespace anchor))

(define (in lvalue rvalue)
  (member lvalue rvalue))
(define (and lvalue rvalue)
  ((eval '(lambda (l r) (and l r)) (make-base-namespace)) lvalue rvalue))
(define (min . values)
  ; Special form of min that returns NaN with no arguments
  ((eval '(lambda (vs) (if (equal? vs '(()))
                           +nan.0
                           (apply min vs)))
         (make-base-namespace)) values))

(define (apply-operator op values)
  (if (not (member op operators))
      (error "Operator not supported:" op)
      (apply (eval op namespace) values)))

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