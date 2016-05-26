#lang racket

(provide json->graph graph->json example-graph) ; TODO: example-graph is an example, remove.

(require json "graph.rkt" "state.rkt" "utilities.rkt")

; A simple graph (tree) with 6 nodes and 5 edges
(define example-graph
  "{\"nodes\": [{\"name\": \"a\"}, {\"name\": \"b\"}, {\"name\": \"c\"}, 
                {\"name\": \"d\"}, {\"name\": \"e\"}, {\"name\": \"f\"}],
    \"links\": [
      {\"source\": 0, \"target\": 1},
      {\"source\": 0, \"target\": 2},
      {\"source\": 1, \"target\": 3},
      {\"source\": 2, \"target\": 4},
      {\"source\": 2, \"target\": 5}]}")

(define (json->graph string)
  (define g (string->jsexpr string))
  (define attributes (make-hash '()))
  (define vertices (build-list (length (hash-ref g 'nodes)) values))
  (let* ([nodes (hash-ref g 'nodes)])  
    (for ([vertex nodes]
          [index (length nodes)])
      (hash-set! attributes index (hash-copy vertex))))
  (define edges (for/list ([edge (hash-ref g 'links)])
                  (let ([pair (cons (hash-ref edge 'source) (hash-ref edge 'target))])
                    (hash-set! attributes pair (hash-copy edge))
                  pair)))
  ;(printf "  Vertices: ~a\n  Edges: ~a\n  Attributes: ~a\n" vertices edges attributes)
  (make-graph vertices edges attributes))

(define (graph->json g state model)
  (define result (make-hash '()))
  (define attr (graph-attributes g))
  (hash-set! result 'nodes (for/list ([index (graph-vertices g)])
                            (hash-ref attr index)))
  (hash-set! result 'edges (for/list ([edge (graph-edges g)])
                            (hash-ref attr edge)))
  (define constraints '())
  (hash-set! result 'constraints (for/list ([c (hash-keys (state-variables state))])
                                   (printf "State variables ~a\n" (state-variables state))
                                   (constraint state model c)))
  (write-json result))

(define (constraint state model c)
  (printf "BEFORE: ~a, ~a, ~a\n" state model c)
  (define val (list-ref constraints (get-value state model c)))
  (printf "Constraint: ~a, ~a\n" c val))
  ;(match val
  ;  ['noop skip]
  ;  ['positional skip]
  ;  ['alignment #hash((axis . "x") (offsets . ) (type . "alignment"))]
  ;  ['grouping skip]))