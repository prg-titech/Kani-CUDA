#lang rosette

(provide make-memory memory-contents memory-length memory-set! global-memory)

;; Model of memory
(struct memory ([length #:mutable] [contents #:mutable]))

;; Create a freash, empty memory with the given capacity
;; or 64 if no capacity is given.
(define (make-memory [capacity 64])
  (memory 0 (make-vector capacity)))

(define (memory-set! mem arr)
  (vector-set! (memory-contents mem) (memory-length mem) arr)
  (set-memory-length! mem (add1 (memory-length mem))))

(define global-memory (make-memory))
  


