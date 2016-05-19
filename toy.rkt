#lang rosette

(require "state.rkt" "graph.rkt" "utilities.rkt" "variables.rkt")

(define vertices (list '1 '2 '3))
(define edges '())
(define attributes #hash())
(define toy-graph (graph vertices edges attributes))

(define program '((def all (== (prop v1 degree) (prop v1 degree))
                   (align x-axis))))

(define (translate graph program)
  (define state (empty-state))
  (map (curry translate-set state graph) program)
  state)

(define (translate-set state graph set)
  (match set
    [`(def ,name ,expression ,statements ...)
     (register-set state name)
     (define relevant-vertices (filter (curry apply-expression graph expression) (vertex-pairs graph)))
     (map (curry apply-statement state graph vertices) (enumerate statements))]))

(define (apply-expression graph expression pair)
  (match expression
    [`(,op ,lvalue ,rvalue) #:when (member op binary-operators)
     (apply-binary-operator op (apply-expression graph lvalue pair)
                               (apply-expression graph rvalue pair))]
    [`(,op ,value) #:when (member op unary-operators)
     (apply-unary-operator op (apply-expression graph value pair))]
    [`(prop ,id degree)     #:when (member id '(v1 v2))
     (vertex-degree graph (if (equal? id 'v1) (car pair) (cdr pair)))]))

(define (apply-statement state graph vertices statement)
  (map (curry apply-statement-pair state graph statement) vertices))

(define (apply-statement-pair state graph statement pair)
  (match statement
    [`(,index align ,axis) #:when (member axis axes)
     (define vconstraint (register-variable state (list 'v1 'v2 index 'constraint)))
     (assert (= vconstraint (list-index 'alignment constraints)))
     (define vaxis (register-variable state (list 'v1 'v2 index 'metadata 0)))
     (assert (= vaxis (list-index axis axes)))]))

(translate toy-graph program)
(asserts)
(solve (asserts))
