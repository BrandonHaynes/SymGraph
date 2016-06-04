#lang rosette

(require "interpreter.rkt" "state.rkt" "graph.rkt" "json.rkt" "variables.rkt" "examples.rkt")

(define program1 '((def layer (= (prop v1 depth) (prop v2 depth))
                   (align x-axis))
                  (def graph (partition (prop v depth))
                   (position y-axis depth))))
(define program1b '((def layer (= (prop v1 depth) (prop v2 depth))
                      (align x-axis)
                      (position x-axis id))
                  (def graph (partition (prop v depth))
                   (position y-axis depth))))
(define program2 '((def layer (= (prop v1 depth) (prop v2 depth))
                     (align x-axis)
                     (position x-axis id))
                   (def child (and (in v1 (prop v2 children))
                                   (= v1 (min (prop v2 children))))
                     (align y-axis))
                   (def graph (partition (prop v depth))
                     (position y-axis depth))))
(define program3 '((def cluster (= (prop v1 depth) (prop v2 depth)))
                   (def graph (cluster)
                     (position y-axis depth (-1 (1 2) 3)))))
(define program4 '((def graph (partition (prop v depth))
                              (position y-axis depth))))

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
(define programFoodWeb
  '((def graph (partition (prop v type))
      (position y-axis type (carnivore herbivore plant)))))

(define programFoodWeb-hard
  '((def cluster (== (prop v1 type) (prop v2 type))
      (position x-axis id)
      (align x-axis))
    (def graph (partition (prop v type))
      (position y-axis type (carnivore herbivore plant)))))

(define test-graph (json->graph food-web))
(printf "-----Translate-----\n")
(define test-state (time (translate test-graph programFoodWeb)))
(printf "\n-------Solve-------\n")
(define test-model (time (solve (asserts))))
(printf "\n------to JSON------\n")
(graph->json test-graph test-state (get-assignments test-model))
