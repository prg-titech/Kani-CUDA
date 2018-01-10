#lang rosette

(require "../../lang.rkt"
         (rename-in rosette/lib/synthax [choose ?])
         racket/hash)

(current-bitwidth 7)

(define (diffusion-kernel in
                          out
                          nx ny nz
                          ce cw cn cs ct cb cc)
  (:= int BLOCKSIZE (*/LS (block-dim 0) (block-dim 1)))
  
  (:= int tid-x (thread-idx 0))
  (:= int tid-y (thread-idx 1))
  
  ; Shared memory
  (:shared real smem[BLOCKSIZE])
  
  (:= int i (+/LS (*/LS (block-dim 0) (block-idx 0)) tid-x))
  (:= int j (+/LS (*/LS (block-dim 1) (block-idx 1)) tid-y))
  
  ; Index of a element to be updated on a global memory 
  (:= int c (+/LS (*/LS j nx) i))
  
  ; Index of a element to be updated on a shared memory
  (:= int c2 (+/LS (*/LS tid-y (block-dim 0)) tid-x))
  
  (:= int xy (*/LS nx ny))
  (: int tb tc tt)
  
  ; Variables for register blocking 
  (= tt [in (+/LS c xy)])
  (= tc [in c])
  (= tb tc)
  
  (= [smem c2] tc)
  
  (:= bool bw (&&/LS (eq?/LS tid-x (? 0 (-/LS (block-dim 0) 1))) (!/LS (eq?/LS i (? 0 (-/LS nx 1))))))
  (:= bool be (&&/LS (eq?/LS tid-x (? 0 (-/LS (block-dim 0) 1))) (!/LS (eq?/LS i (? 0 (-/LS nx 1))))))
  (:= bool bn (&&/LS (eq?/LS tid-y (? 0 (-/LS (block-dim 1) 1))) (!/LS (eq?/LS j (? 0 (-/LS ny 1))))))
  (:= bool bs (&&/LS (eq?/LS tid-y (? 0 (-/LS (block-dim 1) 1))) (!/LS (eq?/LS j (? 0 (-/LS ny 1))))))
  
  (:= int sw (?: (eq?/LS i (? 0 (-/LS nx 1))) c2 (-/LS c2 (? 1 (block-dim 0)))))
  (:= int se (?: (eq?/LS i (? 0 (-/LS nx 1))) c2 (+/LS c2 (? 1 (block-dim 0)))))
  (:= int sn (?: (eq?/LS j (? 0 (-/LS ny 1))) c2 (-/LS c2 (? 1 (block-dim 0)))))
  (:= int ss (?: (eq?/LS j (? 0 (-/LS ny 1))) c2 (+/LS c2 (? 1 (block-dim 0)))))
  
  (= [out c] (+/LS (*/LS cc tc)
                   (*/LS cw (?: bw [in (-/LS c 1)] [smem sw]))
                   (*/LS ce (?: be [in (+/LS c 1)] [smem se]))
                   (*/LS cn (?: bn [in (-/LS c nx)] [smem sn]))
                   (*/LS cs (?: bs [in (+/LS c nx)] [smem ss]))
                   (*/LS cb tb)
                   (*/LS ct tt)))
  
  (+= c xy)
  
  (:= int k 1)
  (for- [: (</LS k (-/LS nz 1)): (++ k)]
        (= tb tc)
        (= tc tt)
        (= [smem c2] tt)
        (= tt [in (+/LS c xy)])
        (= [out c] (+/LS (*/LS cc tc)
                         (*/LS cw (?: bw [in (-/LS c 1)] [smem sw]))
                         (*/LS ce (?: be [in (+/LS c 1)] [smem se]))
                         (*/LS cn (?: bn [in (-/LS c nx)] [smem sn]))
                         (*/LS cs (?: bs [in (+/LS c nx)] [smem ss]))
                         (*/LS cb tb)
                         (*/LS ct tt)))
        (+= c xy))
  
  (= tb tc)
  (= tc tt)
  
  (= [smem c2] tt)
  
  (= [out c] (+/LS (*/LS cc tc)
                   (*/LS cw (?: bw [in (-/LS c 1)] [smem sw]))
                   (*/LS ce (?: be [in (+/LS c 1)] [smem se]))
                   (*/LS cn (?: bn [in (-/LS c nx)] [smem sn]))
                   (*/LS cs (?: bs [in (+/LS c nx)] [smem ss]))
                   (*/LS cb tb)
                   (*/LS ct tt))))