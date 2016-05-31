#lang rosette

(require "variables.rkt")

(provide empty-state register-set register-variable get-value state-variables get-predicate set-count)

(struct state (sets variables))
(struct set-metadata (predicate))

(define (empty-state) (state (make-hash) (make-hash)))

(define (register-set state name predicate)
  (hash-set! (state-sets state) name (set-metadata predicate)))

(define (register-variable state pair index suffix [type integer?])
  (define name (make-name state pair index suffix))
  (when (hash-has-key? (state-variables state) name)
      (error "Duplicate key registered" name (state-variables state)))
  (define variable (make-variable type))
  (hash-set! (state-variables state) name variable)
  variable)

(define (set-count state)
  (hash-count (state-sets state)))

(define (get-predicate state set-name)
  (set-metadata-predicate (hash-ref (state-sets state) set-name)))

(define (get-value state variables variable)
  (hash-ref variables (hash-ref (state-variables state) variable)))

(define (make-name state pair index suffix)
  (append `(,pair ,(set-count state) ,index) suffix))
