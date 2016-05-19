#lang racket

(require json "graph.rkt")

; A simple graph (tree) with 6 nodes and 5 edges
(define s
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
      (hash-set! attributes index vertex)))
  (define edges (for/list ([edge (hash-ref g 'links)])
                  (let ([pair (cons (hash-ref edge 'source) (hash-ref edge 'target))])
                    (hash-set! attributes pair edge)
                  pair)))
  ;(printf "  Vertices: ~a\n  Edges: ~a\n  Attributes: ~a\n" vertices edges attributes)
  (graph vertices edges attributes))

(define (graph->json g model)
  (define result (make-hash '()))
  (define attr (graph-attributes g))
  (hash-set! result 'nodes (for/list ([index (graph-vertices g)])
                            (hash-ref attr index)))
  (hash-set! result 'edges (for/list ([edge (graph-edges g)])
                            (hash-ref attr edge)))
  (define constraints '())
  ;(hash-set! result 'constraints (for/list ([c model])
  ;                                 (constraint c)))
  (write-json result))