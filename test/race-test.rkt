#lang rosette

(require "lang.rkt")

(define arr (make-array (for/vector ([i (in-range 10)]) i)))

(define (test arr)
  (:= int x [arr 0])
  (if- (eq?/LS (tid) (ntid))
         (= [arr 0] 1)))