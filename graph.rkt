#lang racket

(provide make-graph graph-vertices graph-edges graph-attributes vertex-pairs vertex-degree get-attributes get-attribute get-attribute-names)

(struct graph (vertices edges attributes))

(define (make-graph vertices edges attributes)
  (define g (graph vertices edges attributes))
  (generate-depths g)
  (generate-degrees g)
  (generate-children g)
  g)

(define (vertex-pairs graph)
  (cartesian-product (graph-vertices graph)
                     (graph-vertices graph)))

(define (has-attribute? graph key attribute)
   (and
    (hash-has-key? (graph-attributes graph) key)
    (hash-has-key? (get-attributes graph key) attribute)))

(define (get-attributes graph key)
   (hash-ref (graph-attributes graph) key))

(define (get-attribute graph key attribute)
   (hash-ref (get-attributes graph key) attribute))

(define (get-attribute-names graph key)
   ;(hash-keys (hash-ref (graph-attributes graph) key)))
   (hash-keys (get-attributes graph key)))

(define (set-attribute graph key attribute value)
   (hash-set! (hash-ref (graph-attributes graph) key
                        (lambda () (hash-set! (graph-attributes graph) key (make-hash))
                                   (hash-ref  (graph-attributes graph) key)))
              attribute value)
   value)

(define (vertex-degree graph vertex)
  ;(define vertex (list-ref (graph-vertices graph) index))
  (foldl (lambda (edge sum) (+ sum (if (= vertex (car edge)) 1 0)))
         0
         (graph-edges graph)))

(define (graph-root graph)
  ; TODO -- should be using a real root, not a random first vertex
  (car (graph-vertices graph)))

(define (graph-children graph vertex)
  (filter (lambda pair (equal? (car pair) vertex)) (graph-edges graph)))

(define (graph-parents graph vertex)
  (filter (lambda pair (equal? (cdr pair) vertex)) (graph-edges graph)))

(define (generate-depths graph [depth 0] [vertex null])
  (cond
    [(null? vertex)
       (for-each (lambda (v) (set-attribute graph v 'depth -1)) (graph-vertices graph))
       (generate-depths graph 0 (graph-root graph))]
    [(< (get-attribute graph vertex 'depth) 0)
       (set-attribute graph vertex 'depth depth)
       (for-each (curry generate-depths graph (+ depth 1)) (graph-children graph vertex))]
    [else null]))

(define (generate-degrees graph)
  (for-each (lambda (v) (set-attribute graph v 'degree (length (graph-children graph v))))
              (graph-vertices graph)))

(define (generate-parents graph)
  (for-each (lambda (v) (set-attribute graph v 'parents (graph-parents graph v)))
              (graph-vertices graph)))

(define (generate-children graph)
  (for-each (lambda (v) (set-attribute graph v 'children (graph-children graph v)))
              (graph-vertices graph)))
