#lang rosette

(require "../../lang.rkt")

(provide mulmat)

(define (mulmat A B C SIZE file)
  (:= int bdimx (block-dim 0))
  (:= int BLOCKSIZE (*/LS (block-dim 0) (block-dim 1)))
  (:= int row (+/LS (*/LS (block-dim 1) (block-idx 1)) (thread-idx 1)))
  (:= int col (+/LS (*/LS (block-dim 0) (block-idx 0)) (thread-idx 0)))
  (:= int brow (block-idx 1))
  (:= int bcol (block-idx 0))
  (:= int trow (thread-idx 1))
  (:= int tcol (thread-idx 0))
  (:= int c (+/LS (*/LS row SIZE) col))
  (:= int c2 (+/LS tcol (*/LS (block-dim 0) trow)))
  
  (= [C (+/LS (*/LS row SIZE) col)] 0)
  
  (:shared int As[BLOCKSIZE])
  ;(:shared int Bs[BLOCKSIZE])

  (:= int x 0)
  (:= int i 0)
  (for- [:(</LS i (//LS SIZE (block-dim 0))):(+= i 1)]
        (:= int k 0)
        (= [As c2] [A (+/LS (*/LS i (block-dim 0)) tcol (*/LS SIZE row))])
        ;(= [Bs c2] [B (+/LS (*/LS i (*/LS (block-dim 1) SIZE)) (*/LS SIZE trow) col)])
        (syncthreads)
        (for- [:(</LS k (block-dim 0)):(+= k 1)]
              (+= x (*/LS
                     ;desired: trow * bdimx + k
                     ;[A (+/LS (*/LS i (block-dim 0)) k (*/LS SIZE row))]
                     ;(profiling-access file A (+/LS (*/LS i (block-dim 0)) k (*/LS SIZE row))
                     ;                  bdimx c2 row col brow bcol trow tcol i k)
                     (synth-memory-access file A (+/LS (*/LS i (block-dim 0)) k (*/LS SIZE row)) 2)
                     [B (+/LS (*/LS i (*/LS (block-dim 1) SIZE)) (*/LS SIZE k) col)]))
              )
        (syncthreads))
  (= [C (+/LS (*/LS row SIZE) col)]
     (+/LS x [C (+/LS (*/LS row SIZE) col)])))