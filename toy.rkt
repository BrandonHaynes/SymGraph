#lang rosette

(require "state.rkt" "graph.rkt" "utilities.rkt" "variables.rkt")

(define vertices (list '1 '2 '3))
(define edges '())
(define attributes (make-hash))
(define toy-graph (make-graph vertices edges attributes))

(define program1 '((def layer (== (prop v1 depth) (prop v2 depth))
                   (align x-axis))
                  (def graph (layer)
                   (position y-axis depth))))
(define program2 '((def layer (== (prop v1 depth) (prop v2 depth))
                     (align x-axis)
                     (position y-axis depth))
                   (def child (and (in v1 (prop v2 children))
                                   (== v1 (min (prop v2 children))))
                     (align y-axis))
                   (def graph (layer)
                     (position y-axis depth))))
(define program3 '((def cluster (== (prop v1 depth) (prop v2 depth)))
                   (def graph (cluster)
                     (position y-axis depth (-1 (1 2) 3)))))

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
     (map (curry apply-statement state graph vertices) (enumerate statements))]))

(define (equivalence-classes graph predicate)
  (foldl (lambda (v l) (equivalence-class graph predicate v l)) '() (graph-vertices graph)))

(define (equivalence-class graph predicate vertex classes)
  (match (filter (lambda class (apply-expression graph predicate (cons vertex (car class)))) classes)
    [`() (append classes (list (list vertex)))]
    [`(,class _ ...) #:when (not (member vertex class))
        (replace class (cons vertex class) classes)]
    [_ classes]))

;(define (map-id id pair)
;  (if (equal? id 'v1)
;      (car pair)
;      (cadr pair))) ;cdr

(define (apply-expression graph expression pair)
  (match expression
    [`(,op ,lvalue ,rvalue) #:when (member op binary-operators)
     (apply-binary-operator op (apply-expression graph lvalue pair)
                               (apply-expression graph rvalue pair))]
    [`(,op ,value)          #:when (member op unary-operators)
     (apply-unary-operator op (apply-expression graph value pair))]
    ;[`(in ,v ,set)
    ; (apply-set-membership (map-id id pair) (apply-expression graph rvalue pair))]
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
     (define vconstraint (register-variable state `(,pair ,index constraint)))
     (assert (= vconstraint (list-index 'alignment constraints)))
     (define vaxis (register-variable state `(,pair ,index 'metadata 0)))
     (assert (= vaxis (list-index axis axes)))]
    [`(,index position ,axis ,attribute) #:when (member axis axes)
     (define vconstraint (register-variable state `(,pair ,index constraint)))
     (assert (= vconstraint (list-index 'positional constraints)))
     (define vaxis (register-variable state `(,pair ,index 'metadata 0)))
     (assert (= vaxis (list-index axis axes)))
     (define vorder (register-variable state `(,pair ,index 'metadata 1)))
     (assert (= vorder (- (get-attribute graph (car pair) attribute)
                          (get-attribute graph (cadr pair) attribute))))]
    [`(,index position ,axis ,attribute ,order) #:when (member axis axes)
     (define vconstraint (register-variable state `(,pair ,index constraint)))
     (assert (= vconstraint (list-index 'positional constraints)))
     (define vaxis (register-variable state `(,pair ,index 'metadata 0)))
     (assert (= vaxis (list-index axis axes)))
     (define vorder (register-variable state `(,pair ,index 'metadata 1)))
     (assert (= vorder (compare (car pair) (cadr pair) order)))]))


(define s (translate toy-graph program3))
s
(asserts)
(define m (solve (asserts)))
m
(state-variables s)
(printf "Value of '((3 2) 0 constraint) is ~a\n" (get-value s m '((3 2) 0 constraint)))
