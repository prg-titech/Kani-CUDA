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
  (make-vector size (make-memory)))

(define (memory-allocate! arr)
  (set-memory-contents! global-memory (cons arr (memory-contents global-memory))))

(define (shared-memory-allocate! arr)
  (let* ([vec (shared-memory)]
         [smem (vector-ref vec (bid))])
    (set-memory-contents! smem (cons arr (memory-contents smem)))))

(define global-memory (make-memory))

(define shared-memory (make-parameter '#()))