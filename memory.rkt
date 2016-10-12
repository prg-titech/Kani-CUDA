#lang rosette

(require "work.rkt")

(provide make-memory memory-contents memory-length memory-set! global-memory shared-memory-set!)

;; TODO use list
;; Model of memory
(struct memory ([length #:mutable] [contents #:mutable]))

;; Create a freash, empty memory with the given capacity
;; or 64 if no capacity is given.
(define (make-memory [capacity 64])
  (memory 0 (make-vector capacity)))

(define (memory-set! mem arr)
  (vector-set! (memory-contents mem) (memory-length mem) arr)
  (set-memory-length! mem (add1 (memory-length mem))))

(define (shared-memory-set! mem blockid arr)
  (let ([vec (memory-contents mem)])
    (vector-set! vec blockid (cons arr (vector-ref vec blockid)))))

(define global-memory (make-memory))
