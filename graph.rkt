#lang racket

(provide graph vertex-pairs vertex-degree)

(struct graph (vertices edges attributes))

(define (vertex-pairs graph)
  (cartesian-product (graph-vertices graph)
                     (graph-vertices graph)))

;(define (make-graph vertices edges attributes)
;  (map () vertices)
;  )

(define (vertex-degree graph index)
  (define vertex (list-ref (graph-vertices graph) index))
  (printf "~a\n" vertex)
  (printf "~a\n" (graph-edges graph))
  (foldl (lambda (edge sum) (+ sum (if (= vertex (car edge)) 1 0)))
         0
         (graph-edges graph)))
