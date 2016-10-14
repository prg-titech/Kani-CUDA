#lang rosette

(require "work.rkt")

(provide make-memory memory-contents memory-allocate! global-memory shared-memory-set!)

;; TODO use list
;; Model of memory
(struct memory ([contents #:mutable]))

;; Create a freash, empty memory with the given capacity
;; or 64 if no capacity is given.
(define (make-memory)
  (memory null))

(define (memory-allocate! mem arr)
  (set-memory-contents! mem (cons arr (memory-contents mem))))

(define (shared-memory-set! mem blockid arr)
  (let ([vec (memory-contents mem)])
    (vector-set! vec blockid (cons arr (vector-ref vec blockid)))))

(define global-memory (make-memory))
