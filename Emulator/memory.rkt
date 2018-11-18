#lang rosette

(require "work.rkt")

(provide make-memory make-shared-memory memory-contents memory-allocate! global-memory shared-memory shared-memory-allocate!)

;; DONE use list
;; Model of memory for global memory and shared memory.
(struct memory ([contents #:mutable] ; list of array
                ))

;; Create a freash, empty memory
(define (make-memory)
  (memory null))

;; Create a freash, empty shared memory
(define (make-shared-memory size)
  (for/vector ([i size])
    (struct-copy memory (make-memory))))

(define (memory-allocate! arr)
  (set-memory-contents! global-memory (cons arr (memory-contents global-memory))))

(define (shared-memory-allocate! arr)
  (let* ([vec (shared-memory)]
         [smem (vector-ref vec (bid))])
    (set-memory-contents! smem (cons arr (memory-contents smem)))))

(define global-memory (make-memory))

;; Type of shared memory is vector of list of array.
(define shared-memory (make-parameter '#()))
