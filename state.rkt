#lang rosette

(require "variables.rkt")

(provide empty-state register-set register-variable get-value)

(struct state (sets variables))

(define (empty-state) (state (make-hash) (make-hash)))

(define (register-set state name) (hash-set! (state-sets state) name '()))

(define (register-variable state name)
  (define variable (make-variable))
  (hash-set! (state-variables state) name variable)
  variable)

(define (get-value state mod variable)
  (hash-ref (model mod) (hash-ref (state-variables state) variable)))