#lang rosette/safe

(require "graph.rkt" "grammar.rkt" "interpreter.rkt" "variables.rkt" "utilities.rkt")

(define (assign-vertices graph program)
  (define pairs (vertex-pairs graph #:reflexive #f #:symmetric #f))
  (define assignments (map (curry assign-pair graph program) pairs))
  (foldl
   (lambda (predicate aggregate)
     (define relevant-assignments (map cadar (filter (lambda (a) (= (caar a) (car predicate))) assignments)))
     (append (coalesce relevant-assignments (graph-vertices graph)) aggregate))
   '()
   (enumerate (get-predicates program))))

(define (assign-pair graph program pair)
  (map (curry assign-pair-predicate graph pair) (enumerate (get-predicates program))))

(define (assign-pair-predicate graph pair predicate)
  (define v (make-variable boolean?))
  (assert (equal? (interpret-expression graph (cdr predicate) pair) v))
  (list (car predicate) (cons pair v)))

(define (coalesce variables vertices)
  (foldl
    (lambda (v t) (if (contains v t) t (append t (list (list v)))))
    (map remove-duplicates
         (foldl
          (lambda (variable sets)
            (if (cdr variable)
                (cond
                  [(or (contains (caar variable) sets)
                       (contains (cadar variable) sets))
                   (foldl
                    (lambda (s sets)
                      (if (or (member (caar variable) s) (member (cadar variable) s))
                          (append (list (append s (list (caar variable) (cadar variable)))) sets)
                          (append s sets)))
                    '()
                    sets)]
                  [else
                   (append sets (list (car variable)))])
                sets))
          '()
          variables))
    vertices))

(define (contains v lists)
  (ormap (lambda (l) (member v l)) lists))
