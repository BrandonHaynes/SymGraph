#lang racket

(provide json->graph graph->json)

(require json "graph.rkt" "state.rkt" "constraints.rkt" "utilities.rkt")

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
  (hash-set! result 'links (for/list ([edge (graph-edges g)])
                            (hash-ref attr edge)))
  (define constraints '())
  (hash-set! result 'constraints (filter-not void? (for/list ([c (hash-keys (state-variables state))])
                                   ;(printf "State variables ~a\n" (state-variables state))
                                   (match c
                                     [(list _ _ _ 'constraint) (constraint g state model c)]
                                     [_ (void)]))))
  (write-json result))
  ;(define out (open-output-file "graph.json" #:exists 'replace))
  ;(write output-json out)
  ;(close-output-port out))

(define (constraint graph state model c)
  (define nodes (list-ref c 0))
  (define set-index (list-ref c 1))
  (define constraint-index (list-ref c 2))
  (define val (list-ref constraints (get-value state model c)))
  ;(printf "Constraint: ~a, ~a\n" c val)
  (match val
    ['noop (void)]
    ; Transforming a positional constraint
    ['positional
     (define result (make-hasheq '((gap . 10))))
     (let* ([direction (get-value state model (list nodes set-index constraint-index 'metadata 1))])
       ;(printf "Metadata1: ~a \tNodes: ~a\n" direction nodes)
       (if (< direction 0)
           (begin (hash-set! result 'left (list-index (car nodes) (graph-vertices graph)))
                  (hash-set! result 'right (list-index (list-ref (cdr nodes) 0) (graph-vertices graph))))
           (begin (hash-set! result 'right (list-index (car nodes) (graph-vertices graph)))
                  (hash-set! result 'left (list-index (list-ref (cdr nodes) 0) (graph-vertices graph))))))
     ;(printf "VALUE: ~a -- ~a -- ~a\n" (list-ref axes (get-value state model (list nodes set-index constraint-index 'metadata 0)))
     ;        (get-value state model (list nodes set-index constraint-index 'metadata 0))
     ;        (list nodes set-index constraint-index 'metadata 0))
     (define axis-val (match (list-ref axes (get-value state model (list nodes set-index constraint-index 'metadata 0)))
                        ['x-axis "x"]
                        ['y-axis "y"]))
     ;(printf "Axis: ~a\n" axis-val)
     (hash-set! result 'axis axis-val)
     result]
    ; Transforming an alignment constraint
    ['alignment
     (define node1 (make-hasheq '((offset . 0))))
     (hash-set! node1 'node (list-index (car nodes) (graph-vertices graph)))
     (define node2 (make-hasheq '((offset . 0))))
     (hash-set! node2 'node (list-index (list-ref (cdr nodes) 0) (graph-vertices graph)))
     (define offsets  (list node1 node2)) 
     (define result (make-hasheq '((type . "alignment"))))
     (define axis-val (match (list-ref axes (get-value state model (list nodes set-index constraint-index 'metadata 0)))
                        ['x-axis "y"]
                        ['y-axis "x"]))
     ;(printf "Axis: ~a\n" axis-val)
     (hash-set! result 'axis axis-val)
     (hash-set! result 'offsets offsets)
     result]
    ['grouping (void)]))