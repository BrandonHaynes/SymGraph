#lang racket

(provide program1 program2 programFoodWeb programFoodWeb-hard)

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

(define programFoodWeb
  '((def graph (partition (prop v type))
      (position y-axis type (carnivore herbivore plant)))))

(define programFoodWeb-hard
  '((def cluster (== (prop v1 type) (prop v2 type))
      (position x-axis id)
      (align x-axis))
    (def graph (partition (prop v type))
      (position y-axis type (carnivore herbivore plant)))))