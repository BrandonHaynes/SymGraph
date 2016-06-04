#lang rosette

(require "interpreter.rkt" "state.rkt" "graph.rkt" "json.rkt" "variables.rkt" "examples.rkt" "programs.rkt")

;(define s (translate toy-graph program3))
;s
;(asserts)
;(define m (solve (asserts)))
;m
;(state-variables s)
;(printf "Value of '((3 2) 0 constraint) is ~a\n" (get-value s (get-assignments m) '((3 2) 1 1 constraint)))
;(printf "Nodes: ~a\n" (graph-vertices test-graph))
;(printf "Edges: ~a\n" (graph-edges test-graph))
;(asserts)
;(printf "State variables\n")
;(for-each (lambda (v) (printf "~a = ~a\n" v (get-value test-state (get-assignments test-model) (car v)))) (hash->list (state-variables test-state)))


; Demo Example:
(define test-graph (json->graph food-web))
(printf "-----Translate-----\n")
(define test-state (time (translate test-graph programFoodWeb-hard)))
(printf "\n-------Solve-------\n")
(define test-model (time (solve (asserts))))
(printf "\n------to JSON------\n")
(graph->json test-graph test-state (get-assignments test-model))
