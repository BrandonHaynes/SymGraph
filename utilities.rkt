#lang rosette

(provide list-index enumerate replace compare bit-set?)

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
  (printf "~a is in group ~a, ~a is in group ~a -- result ~a\n" left left-group right right-group (cond
    [(equal? left-group right-group) 0]
    [null? left-group -1]
    [((get-comparator left) left-group right-group) -1]
    [else 1]))
  (cond
    [(equal? left-group right-group) 0]
    [null? left-group -1]
    [((get-comparator left) left-group right-group) -1]
    [else 1]))

(define (order-group value order)
  ;(printf "~a ~a\n" value order)
  (cond
    [(empty? order) null]
    [((get-comparator value) value (car (flatten order))) (car order)]
    [else (order-group value (cdr order))]))

(define (get-comparator value)
  (cond
    [(number? value) <]
    [(string? value) string<?]
    [(symbol? value) symbol<?]
    ))

(define (bit-set? b index size)
  (bvuge (bvand b (bvshl (bv 1 size) (bv (- index 1) size))) (bv 1 size)))