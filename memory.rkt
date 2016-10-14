#lang rosette

(require "work.rkt")

(provide make-memory make-shared-memory memory-contents memory-allocate! global-memory shared-memory shared-memory-allocate!)

;; TODO use list
;; Model of memory
(struct memory ([contents #:mutable]))

;; Create a freash, empty memory with the given capacity
;; or 64 if no capacity is given.
(define (make-memory)
  (memory null))

(define (make-shared-memory size)
  (memory (make-vector (block-size))))

(define (memory-allocate! arr)
  (set-memory-contents! global-memory (cons arr (memory-contents global-memory))))

(define (shared-memory-allocate! blockid arr)
  (let ([vec (memory-contents (shared-memory))])
    (vector-set! vec blockid (cons arr (vector-ref vec blockid)))))

(define global-memory (make-memory))

(define shared-memory (make-parameter '#()))






