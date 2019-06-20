#lang rosette

(require rackunit
         "../lang.rkt")

(define BLOCKSIZE 16)

(define (main)
  (: int [ans BLOCKSIZE])
  (for ([i BLOCKSIZE])
    (array-set-host! ans i (+ i 1)))
  
  (: int [arr BLOCKSIZE])
  (for ([i BLOCKSIZE])
    (array-set-host! arr i 0))

  (:* float dev_a0)
  (cudaMalloc dev_a0 16)
  (print-matrix dev_a0 4 4)
  
  
  (invoke-kernel add-tid '(1) (list BLOCKSIZE) arr)
  (print-matrix arr 4 4)
  ;(for ([i BLOCKSIZE])
  ;  (check-eq? (array-ref-host arr i) (array-ref-host ans i)))
  )

(define (add-tid arr)
  (for- [(:= int i 0):(</LS i (+/LS (thread-idx 0) 1)):(+=/LS i 1)]
        ;(println [arr (thread-idx 0)])
        (= [arr (thread-idx 0)] (+/LS [arr (thread-idx 0)] 1))
        ;(println [arr (thread-idx 0)])
        ))

(main)
