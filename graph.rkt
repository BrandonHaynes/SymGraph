#lang racket

(provide graph graph-vertices graph-edges graph-attributes vertex-pairs vertex-degree)

(struct graph (vertices edges attributes))

(define (vertex-pairs graph)
  (cartesian-product (graph-vertices graph)
                     (graph-vertices graph)))

(define (get-attribute g key attribute)
   (hash-ref (hash-ref (graph-attributes g) key) attribute))

(define (vertex-degree graph vertex)
  ;(define vertex (list-ref (graph-vertices graph) index))
  (foldl (lambda (edge sum) (+ sum (if (= vertex (car edge)) 1 0)))
         0
         (graph-edges graph)))
