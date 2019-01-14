#lang rosette

(require "../lang.rkt")

(define arr (make-array (for/vector ([i (in-range 10)]) (make-element 0)) 10))

(define (test arr)
  (= [arr 0] 1)
  [arr 0])

(define (test-block-race arr)
  (: int x) 
  (if (eq? (block-idx 0) 0)
       (begin
         (if- (eq?/LS (thread-idx 0) 0)
              (= [arr 0] 4))
         ;(syncthreads)
         (if- (eq?/LS (thread-idx 0) 1)
              (= x [arr 0])))
       (= x [arr 0])))

(invoke-kernel test-block-race '(2) '(5) arr)

(print-matrix arr 10 1)