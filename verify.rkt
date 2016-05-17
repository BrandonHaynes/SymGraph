#lang rosette/safe

(require rosette/lib/synthax)

(struct node (id))

;(define (xnode? x) #t)
(define-symbolic i integer?)

;(apply define-symbolic (list a b c) integer?)

(define constraints '(noop positional alignment grouping))

(define-symbolic a b c d integer?)
(define-symbolic f (~> integer? boolean?))

(define-symbolic c.ab integer?)
(assert (>= c.ab 0))
(assert (< c.ab (length constraints)))

;(define c.ab (constant 'c.ab integer?))
;(define foo 'q)
;(define-symbolic (foo) integer?)
;(define (always-same z)
;    (define-symbolic (string->symbol z) integer?)
;    z)
;(always-same 'q)
(define graph-size 3)
;(define max-statements 2)
;(define total-variables (* graph-size graph-size max-statements))

(define (make-variable)
  (define-symbolic* v integer?)
  (assert (>= v 0))
  (assert (<  v (length constraints)))
  v)
(define (make-variables n)  
  (append (list (make-variable)) (if (> n 1) (make-variables (- n 1)) '())))
;(map foobar '(a b c))
;(define variables (make-variables total-variables))
;(define (get-variable vertex1 vertex2 constraint) (+ (* vertex1 vertex2) constraint))

;(define foo (lambda (x) (define-symbolic x integer?)))
;(map (lambda (x) (define-symbolic x integer?)) '(1 2 3))

;(define-symbolic 

;(define (x) (list a b))
;(define (y) (list c d))

(define (desired-constraint statements x y)
  ; TODO: recurse
  (define statement (if (empty? statements) null (car statements)))
  (cond
    [(null? statement) '()]
    [(eq? (car statement) 'align) 2]))

(define (list-of-constraint-statements x)
  '((align x x-axis))) ; align on x axis with everyone else in my set

(define (constrain-x-y cc x y)
  (define dc (desired-constraint (list-of-constraint-statements x) x y))
  (assert (not (= cc dc))))

;(define cexab (verify (constrain-x-y c.ab a b)))
(define cex (verify (constrain-x-y (get-variable 0 0 0)
                                   (get-variable 0 0 1)
                                   (get-variable 0 0 2))))

;(define binding
;    (synthesize #:forall (list c.ab a b c d)
;                #:guarantee (constrain-x-y c.ab a b)))

;(print-forms binding)
(printf "cex ~a\n" cex)
;(printf "cexab ~a\n" cexab)
(asserts)
(evaluate (get-variable 0 0 0) cex)
;(evaluate c.ab cexab)
;(constrain-x-y (evaluate c.ab cex) 1 1)