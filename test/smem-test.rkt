#lang rosette

(require "../lang.rkt")

(define BDIMX 4)
(define BDIMY 4)

(define (test-smem src)
  (:shared int arr[10][1])
  (:= int ix (+/LS (tid) (*/LS (bid) (thread-dim))))
  (= [arr (tid) 0] [src ix])
  (barrier)
  (print [arr 0 0]))

(define (test-smem-rectangle out)
  (:shared int tile[BDIMY][BDIMX])
  (:= int irow (quotient/LS (tid) BDIMY))
  (:= int icol (modulo/LS (tid) BDIMY))
  (= [tile icol irow] (tid))
  (barrier)
  (= [out (tid)] [tile irow icol]))

(define out (make-array (for/vector ([i (in-range 30)]) (make-element i)) 30))

(invoke-kernel test-smem-rectangle 1 16 out)

(printmatrix out 4 4)

;(define src (make-array (for/vector ([i (in-range 30)]) (make-element i)) 30))

  
;(invoke-kernel test-smem 3 10 src)