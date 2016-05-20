#lang rosette

(require "state.rkt" "graph.rkt" "utilities.rkt" "variables.rkt")

(define vertices (list '1 '2 '3))
(define edges '())
(define attributes (make-hash))
(define toy-graph (make-graph vertices edges attributes))

(define program '((def layer (== (prop v1 depth) (prop v1 depth))
                   (align x-axis))
                  (def graph (layer)
                   (align y-axis))))

(define (translate graph program)
  (define state (empty-state))
  (map (curry translate-set state graph) program)
  state)

(define (translate-set state graph set)
  (match set
    [`(def ,name (,ids ...) ,statements ...) #:when (andmap symbol? ids)
                          (null)] ; partition by sets, do all that nonsense
    [`(def ,name ,expression ,statements ...)
     (register-set state name)
     (define relevant-vertices (filter (curry apply-expression graph expression) (vertex-pairs graph)))
     (map (curry apply-statement state graph vertices) (enumerate statements))]))

(define (map-id id pair)
  (if (equal? id 'v1)
      (car pair)
      (cdr pair)))

(define (apply-expression graph expression pair)
  (match expression
    [`(,op ,lvalue ,rvalue) #:when (member op binary-operators)
     (apply-binary-operator op (apply-expression graph lvalue pair)
                               (apply-expression graph rvalue pair))]
    [`(,op ,value)          #:when (member op unary-operators)
     (apply-unary-operator op (apply-expression graph value pair))]
    [`(prop ,id ,attribute) #:when (and (member id '(v1 v2))
                                        (member attribute (get-attribute-names graph (map-id id pair))))
     (get-attribute graph (map-id id pair) attribute)]))

(define (apply-statement state graph vertices statement)
  (map (curry apply-statement-pair state graph statement)
       (filter (lambda (pair) (not (equal? (car pair) (cadr pair))))
               (cartesian-product vertices vertices))))

(define (apply-statement-pair state graph statement pair)
  (match statement
    [`(,index align ,axis) #:when (member axis axes)
     (define vconstraint (register-variable state `(,pair ,index constraint)))
     (assert (= vconstraint (list-index 'alignment constraints)))
     (define vaxis (register-variable state `(,pair ,index 'metadata 0)))
     (assert (= vaxis (list-index axis axes)))]))

(define s (translate toy-graph program))
s
(asserts)
(define m (solve (asserts)))
m
(printf "Value of '((3 2) 0 constraint) is ~a\n" (get-value s m '((3 2) 0 constraint)))
