#lang rosette

(require "work.rkt" "array.rkt" "memory.rkt")

(provide barrier)

;; Synchronize memory
(define (memory-synchronize! mem)
  (define cont (memory-contents mem))
  (for ([i (in-range 0 (length cont))])
    (let ([vec (array-contents (list-ref cont i))])
    (begin
      (vector-map! read-reset! vec)
      (vector-map! write-reset! vec)))))

;; Barrier divergence check
;; When the execution reach a barrier, we need to check that all 
;; threads are participate in this barrier
(define (barrier-ok m)
  (or (for/and ([x m]) x)
      (for/and ([x m]) (! x))))

;; Barrier
;; Just do the barrier divergence check
(define (barrier)
  (memory-synchronize! global-memory)
  (memory-synchronize! (vector-ref (shared-memory) (bid)))
  (let ([m (mask)])
    (assert (barrier-ok m))))
