#lang racket

(require "state.rkt" "graph.rkt" "utilities.rkt" "variables.rkt" "constraints.rkt" "json.rkt")

(provide translate)

(define (translate graph program)
  (define state (empty-state))
  (map (curry translate-set state graph) program)
  state)

(define (translate-set state graph set)
  (match set
    [`(def ,name (,reference-name _ ...) ,statements ...) #:when (symbol? reference-name) ;#:when (andmap symbol? reference-name)
     (map 
        (lambda (vertices) (map (curry apply-statement state graph vertices) (enumerate statements)))
        (equivalence-classes graph (get-predicate state reference-name)))] ; partition by sets
    [`(def ,name ,predicate ,statements ...)
     (register-set state name predicate)
     (define relevant-vertices (filter (curry apply-expression graph predicate) (vertex-pairs graph)))
     (map (curry apply-statement state graph (graph-vertices graph))
          (enumerate statements))]))

(define (equivalence-classes graph predicate)
  (foldl (lambda (v l) (equivalence-class graph predicate v l)) '() (graph-vertices graph)))

(define (equivalence-class graph predicate vertex classes)
  (match (filter (lambda class (apply-expression graph predicate (cons vertex (car class)))) classes)
    [`() (append classes (list (list vertex)))]
    [`(,class _ ...) #:when (not (member vertex class))
        (replace class (cons vertex class) classes)]
    [_ classes]))

(define (apply-expression graph expression pair)
  (match expression
    [`(,op ,lvalue ,rvalue) #:when (member op binary-operators)
     (apply-binary-operator op (apply-expression graph lvalue pair)
                               (apply-expression graph rvalue pair))]
    [`(,op ,value)          #:when (member op unary-operators)
     (apply-unary-operator op (apply-expression graph value pair))]
    [`(prop ,id ,attribute) #:when (and (member id '(v1 v2))
                                        (member attribute (get-attribute-names graph (apply-expression graph id pair))))
     (get-attribute graph (apply-expression graph id pair) attribute)]
    ['v1 (car pair)]
    ['v2 (cadr pair)]))

(define (apply-statement state graph vertices statement)
  (map (curry apply-statement-pair state graph statement)
       (filter (lambda (pair) (not (equal? (car pair) (cadr pair))))
               (cartesian-product vertices vertices))))

(define (apply-statement-pair state graph statement pair)
  (match statement
    [`(,index align ,axis) #:when (member axis axes)
     (align-constraint state axis index pair)]
    [`(,index position ,axis ,attribute) #:when (member axis axes)
     (position-constraint state graph axis attribute index pair)]
    [`(,index position ,axis ,attribute ,order) #:when (member axis axes)
     (ordered-position-constraint state axis order index pair)]))
