#lang racket

(require "variables.rkt")

(provide empty-state register-set register-variable get-value state-variables get-predicate)

(struct state (sets variables))
(struct set-metadata (predicate))

(define (empty-state) (state (make-hash) (make-hash)))

(define (register-set state name predicate)
  (hash-set! (state-sets state) name (set-metadata predicate)))

(define (register-variable state name)
  (define variable (make-variable))
  (hash-set! (state-variables state) name variable)
  variable)

(define (get-predicate state set-name)
  (set-metadata-predicate (hash-ref (state-sets state) set-name)))

(define (get-value state variables variable)
  (hash-ref variables (hash-ref (state-variables state) variable)))