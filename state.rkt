#lang racket

(require "variables.rkt")

(provide empty-state register-set register-variable)

(struct state (sets variables))

(define (empty-state) (state (make-hash) (make-hash)))

(define (register-set state name) (hash-set! (state-sets state) name '()))

(define (register-variable state name)
  (define variable (make-variable))
  (hash-set! (state-variables state) name variable)
  variable)