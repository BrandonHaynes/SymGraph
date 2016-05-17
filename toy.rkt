#lang racket

(require "utilities.rkt")

(define vertices (list '1 '2 '3))

(define program '((def all (= (prop v1 degree) (prop v1 degree))
                   (align x-axis))))

(define (translate program)
  (map translate-set program))

(define (translate-set set)
  (match set
    [`(def ,name ,expression ,statements ...)
     'foobarbaz]))

(printf "~a" (translate program))
