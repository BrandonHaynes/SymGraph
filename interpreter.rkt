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
    [`(def ,name (partition ,expression) ,statements ...)
        (define classes (equivalence-classes graph expression))
        (define class-product (apply cartesian-product classes))
            (printf "eq: ~a\n" classes)
            (printf "cp: ~a\n" class-product)
        (for-each
          (lambda (vertices)
            (define relevant-vertices (filter (lambda (pair) (< (interpret-expression graph expression (cons (car pair) (car pair)))
                                                                (interpret-expression graph expression (cons (cadr pair) (cadr pair)))))
                                              (cartesian-product vertices vertices)))
            (printf "r: ~a\n" (car relevant-vertices))
            (for-each
             (lambda (s) (for-each (lambda (p) (apply-statement-pair state graph s p)) relevant-vertices))
             (enumerate statements (* (car vertices) (length statements))))
            )
          class-product)
     ]
    [`(def ,name (,reference-name _ ...) ,statements ...) #:when (symbol? reference-name) ;#:when (andmap symbol? reference-name)
     (map 
        (lambda (vertices) (map (curry apply-statement state graph (cdr vertices)) (enumerate statements (* (car vertices) (length statements)))))
        (enumerate (equivalence-classes graph (get-predicate state reference-name))))] ; partition by sets
    [`(def ,name ,predicate ,statements ...)
     (register-set state name predicate)
     (define relevant-vertices (filter (curry interpret-expression graph predicate) (vertex-pairs graph #:reflexive #f)))
     (map (curry apply-statement state graph relevant-vertices)
          (enumerate statements))]))

(define (equivalence-classes graph expression)
  (sort
    (foldl (lambda (v l) (equivalence-class graph expression v l)) '() (graph-vertices graph))
    >
    #:key (lambda (c) (interpret-expression graph expression (cons (car c) (car c))))
    ))

(define (equivalence-class graph expression vertex classes)
  (match (filter (lambda (class) (equal? (interpret-expression graph expression (cons vertex vertex))
                                         (interpret-expression graph expression (cons (car class) (car class))))) classes)
    [`() (append classes (list (list vertex)))]
    [`(,class _ ...) #:when (not (member vertex class))
        (replace class (cons vertex class) classes)]
    [_ classes]))

(define (interpret-expression graph expression pair)
  (match expression
    [`(,op ,values ...) #:when (member op operators)
     (apply-operator op (map (lambda (v) (interpret-expression graph v pair)) values))]
    [`(prop ,id ,attribute) #:when (and (member id '(v1 v2 v))
                                        (member attribute (get-attribute-names graph (interpret-expression graph id pair))))
     (get-attribute graph (interpret-expression graph id pair) attribute)]
    ['v #:when (equal? (car pair) (cdr pair)) (car pair)]
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


(define vertices '(1 2 3 4))
(define edges '((1 . 2)))
(define toy-graph (make-graph vertices edges))

(define program4 '((def graph (partition (prop v depth))
                              (position y-axis depth))))

(define test-state (translate toy-graph program4))

