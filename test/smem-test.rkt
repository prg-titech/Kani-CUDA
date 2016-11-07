#lang rosette

(require "../lang.rkt")

(define BDIMX 4)
(define BDIMY 4)

(define (test-smem src)
  (:shared int arr[16][1])
  (:= int ix (+/LS (thread-idx 0) (*/LS (block-idx 0) (block-size))))
  (= [arr (thread-idx 0) 0] [src ix])
  (barrier)
  (print [arr 0 0]))

(define (test-smem-rectangle out)
  (:shared int tile[BDIMY][BDIMX])
  (:= int irow (quotient/LS (thread-idx 0) BDIMY))
  (:= int icol (modulo/LS (thread-idx 0) BDIMY))
  (= [tile icol irow] (thread-idx 0))
  (barrier)
  (= [out (thread-idx 0)] [tile irow icol]))

(define out (make-array (for/vector ([i (in-range 16)]) (make-element 0)) 16))

(invoke-kernel test-smem-rectangle '(1) '(16) out)

(printmatrix out 4 4)

;(define src (make-array (for/vector ([i (in-range 30)]) (make-element i)) 30))

  
;(invoke-kernel test-smem 3 10 src)