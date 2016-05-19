#lang racket

(provide empty-state register-set)

(struct state (sets))

(define (empty-state) (state (make-hash)))

(define (register-set state name) (hash-set! (state-sets state) name '()))