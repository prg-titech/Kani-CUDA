#lang rosette

(require "../../lang.rkt")

(provide vec-add)

(func void vec-add ((int A) (int B) (int C) (int SIZE))
  (:= int tid-x (thread-idx 0))
  (:= int tid-y (thread-idx 1))
  (:= int bdim-x (block-dim 0))
  (:= int bdim-y (block-dim 1))
  (:= int BLOCKSIZE (*/LS (block-dim 0) (block-dim 1)))
  (:= int c (+/LS (thread-idx 0) (*/LS (thread-idx 1) (block-dim 0))))
  (:shared int [As BLOCKSIZE])
  (= [As c] [A c])
  (syncthreads)
  (= [C c] (+/LS [A c] [B c])))