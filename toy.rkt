#lang rosette/safe

(require "core.rkt" "state.rkt" "graph.rkt" "json.rkt" "variables.rkt")

(define vertices (list '1 '2 '3))
(define edges '((1 . 2)))
(define toy-graph (make-graph vertices edges))

(graph-attributes toy-graph)

(define program1 '((def layer (= (prop v1 depth) (prop v2 depth))
                   (align x-axis))
                  (def graph (layer)
                   (position y-axis depth))))
(define program2 '((def layer (= (prop v1 depth) (prop v2 depth))
                     (align x-axis)
                     (position y-axis depth))
                   (def child (and (in v1 (prop v2 children))
                                   (= v1 (min (prop v2 children))))
                     (align y-axis))
                   (def graph (layer)
                     (position y-axis depth))))
(define program3 '((def cluster (= (prop v1 depth) (prop v2 depth)))
                   (def graph (cluster)
                     (position y-axis depth (-1 (1 2) 3)))))

(define s (translate toy-graph program3))
s
(asserts)
(define m (solve (asserts)))
m
(state-variables s)
(printf "Value of '((3 2) 0 constraint) is ~a\n" (get-value s (get-assignments m) '((3 2) 0 constraint)))

; Testing for the JSON
(define test-graph (json->graph example-graph))
(printf "Nodes: ~a\n" (graph-vertices test-graph))
(printf "Edges: ~a\n" (graph-edges test-graph))
(define test-state (translate test-graph program2))
(asserts)
(define test-model (solve (asserts)))
(printf "State variables ~a\n" (state-variables test-state))
(printf "~a\n" (get-assignments test-model))
(graph->json test-graph test-state (get-assignments test-model))
