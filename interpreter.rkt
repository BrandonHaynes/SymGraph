#lang rosette

(require "state.rkt" "graph.rkt" "utilities.rkt" "constraints.rkt")

(provide translate interpret-expression)

(define-namespace-anchor anchor)
(define namespace (namespace-anchor->namespace anchor))

(define operators '(= <= >= < > and or in not min))

(define (translate graph program)
  (define state (empty-state))
  (map (curry translate-set state graph) program)
  state)

(define (translate-set state graph set)
  (match set
    [`(def ,name (,reference-name _ ...) ,statements ...) #:when (symbol? reference-name) ;#:when (andmap symbol? reference-name)
     (map 
        (lambda (vertices) (map (curry apply-statement state graph (cdr vertices)) (enumerate statements (* (car vertices) (length statements)))))
        (enumerate (equivalence-classes graph (get-predicate state reference-name))))] ; partition by sets
    [`(def ,name ,predicate ,statements ...)
     (register-set state name predicate)
     (define relevant-vertices (filter (curry interpret-expression graph predicate) (vertex-pairs graph #:reflexive #f)))
     (map (curry apply-statement state graph relevant-vertices)
          (enumerate statements))]))

(define (equivalence-classes graph predicate)
  (foldl (lambda (v l) (equivalence-class graph predicate v l)) '() (graph-vertices graph)))

(define (equivalence-class graph predicate vertex classes)
  (match (filter (lambda class (interpret-expression graph predicate (cons vertex (car class)))) classes)
    [`() (append classes (list (list vertex)))]
    [`(,class _ ...) #:when (not (member vertex class))
        (replace class (cons vertex class) classes)]
    [_ classes]))

(define (interpret-expression graph expression pair)
  (match expression
    [`(,op ,values ...) #:when (member op operators)
     (apply-operator op (map (lambda (v) (interpret-expression graph v pair)) values))]
    [`(prop ,id ,attribute) #:when (and (member id '(v1 v2))
                                        (member attribute (get-attribute-names graph (interpret-expression graph id pair))))
     (get-attribute graph (interpret-expression graph id pair) attribute)]
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

(define (apply-operator op values)
  (if (not (member op operators))
      (error "Operator not supported:" op)
      (apply (eval op namespace) values)))

(define (in lvalue rvalue)
  (member lvalue rvalue))
(define (and lvalue rvalue)
  ((eval '(lambda (l r) (and l r)) (make-base-namespace)) lvalue rvalue))
(define (min values)
  ; Special form of min that returns NaN with no arguments
  (if (equal? values '()) +nan.0
      ((eval '(lambda (vs) (apply min vs))
             (make-base-namespace)) values)))
