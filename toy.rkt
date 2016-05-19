#lang racket

(require "state.rkt" "graph.rkt" "utilities.rkt")

(define vertices (list '1 '2 '3))
(define edges '())
(define attributes #hash())
(define toy-graph (graph vertices edges attributes))

(define program '((def all (== (prop v1 degree) (prop v1 degree))
                   (align x-axis))))

(define (translate graph program)
  (map (curry translate-set (empty-state) graph) program))

(define (translate-set state graph set)
  (match set
    [`(def ,name ,expression ,statements ...)
     (register-set state name)
     (filter (curry execute-expression graph expression) (vertex-pairs graph))
     '()]
    [`(,op ,lvalue ,rvalue) #:when (member op operators)
     (list op (translate-set state graph lvalue) (translate-set state graph rvalue))]
    ))

(define (execute-expression graph expression pair)
  (match expression
    [`(,op ,lvalue ,rvalue) #:when (member op operators)
     (apply-operator op (execute-expression graph lvalue pair)
                        (execute-expression graph rvalue pair))]
    [`(prop ,id degree)     #:when (member id '(v1 v2))
     (vertex-degree graph (if (equal? id 'v1) (car pair) (cdr pair)))]))

(printf "~a" (translate toy-graph program))
