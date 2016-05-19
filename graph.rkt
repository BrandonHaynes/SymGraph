#lang racket

(require json)

(provide graph vertex-pairs vertex-degree)

(struct graph (vertices edges attributes))

(define (vertex-pairs graph)
  (cartesian-product (graph-vertices graph)
                     (graph-vertices graph)))

(define s "{\"nodes\": [{\"name\": \"a\"}, {\"name\": \"b\"}, {\"name\": \"c\"}, 
{\"name\": \"d\"}, {\"name\": \"e\"}, {\"name\": \"f\"}],
 \"links\": [
    {\"source\": 0, \"target\": 1},
    {\"source\": 0, \"target\": 2},
    {\"source\": 1, \"target\": 3},
    {\"source\": 2, \"target\": 4},
    {\"source\": 2, \"target\": 5}
  ]}")

(define (json->graph string)
  (define g (string->jsexpr string))
  (define attributes (make-hash '()))
  (define vertices (build-list (length (hash-ref g 'nodes)) values))
  (let* ([nodes (hash-ref g 'nodes)])  
    (for ([vertex nodes]
          [index (length nodes)])
      (hash-set! attributes index vertex)))
  (define edges (for/list ([edge (hash-ref g 'links)])
                  (let ([pair (cons (hash-ref edge 'source) (hash-ref edge 'target))])
                    (hash-set! attributes pair edge)
                  pair)))
  ;(printf "  Vertices: ~a\n  Edges: ~a\n  Attributes: ~a\n" vertices edges attributes)
  (graph vertices edges attributes))

(define (get-attribute g key attribute)
   (hash-ref (hash-ref (graph-attributes g) key) attribute))

(define (vertex-degree graph vertex)
  ;(define vertex (list-ref (graph-vertices graph) index))
  (foldl (lambda (edge sum) (+ sum (if (= vertex (car edge)) 1 0)))
         0
         (graph-edges graph)))
