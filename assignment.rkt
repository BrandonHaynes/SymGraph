#lang rosette/safe

(require "graph.rkt" "state.rkt" "grammar.rkt" "interpreter.rkt" "utilities.rkt")

(define (assign-vertices graph program)
  (define state (empty-state))
  (define pairs (vertex-pairs graph #:reflexive #f #:symmetric #f))
  (for-each (curry assign-pair state graph program) pairs)
  state)

(define (assign-pair state graph program pair)
  (for-each (curry assign-pair-predicate state graph pair) (enumerate (get-predicates program))))

(define (assign-pair-predicate state graph pair predicate)
  (define v (register-variable state pair (car predicate) '(pair) boolean?))
  (assert (equal? (interpret-expression graph (cdr predicate) pair) v)))

(define program1 '((def layer (= (prop v1 depth) (prop v2 depth))
                   (align x-axis))
                  ));(def graph (layer)
                   ;(position y-axis depth))))

(define vertices (list '1 '2 '3))
(define edges '())
(define toy-graph (make-graph vertices edges))

(define state (assign-vertices toy-graph program1))
(solve (asserts))
