#lang rosette

(require "../lang.rkt")

(define (test-smem src)
  (:shared int arr[10][1])
  (:= int ix (+/LS (tid) (*/LS (bid) (thread-dim))))
  (= [arr (tid) 0] [src ix])
  (barrier)
  (print [arr 0 0])
)

(define src (make-array (for/vector ([i (in-range 30)]) (make-element i)) 30))

(invoke-kernel test-smem 3 10 src)