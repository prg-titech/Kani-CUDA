#lang rosette

(require "../../lang.rkt")

(provide convo)

(define (convo o-data i-data d-kernel dataw datah file)
  (:= int KERNEL-RADIUS 2)
  (:= int tidx (thread-idx 0))
  (:= int tidy (thread-idx 1))
  (:= int bdimx (block-dim 0))
  (:= int bdimy (block-dim 1))
  (:= int BLOCKSIZE (*/LS bdimx bdimy))
  (:= int bidx (block-idx 0))
  (:= int bidy (block-idx 1))
  (:= int gdimx (grid-dim 0))
  (:= int gdimy (grid-dim 1))
  (:= int b-loc (+/LS tidx
                      (*/LS tidy bdimx)))
  (:= int g-loc (+/LS tidx
                      (*/LS bidx bdimx)
                      (*/LS tidy dataw)
                      (*/LS bidy bdimy dataw)))
  
  (:= int sum 0)
  (:= int value 0)
  (:shared int sm[BLOCKSIZE])
  (= [sm b-loc] [i-data g-loc])

  (syncthreads)
  
  (:= int i (*/LS -1 KERNEL-RADIUS))
  (for- [:(</LS i (+/LS KERNEL-RADIUS 1)):(+= i 1)]
        (:= int j (*/LS -1 KERNEL-RADIUS))
        (for- [:(</LS j (+/LS KERNEL-RADIUS 1)):(+= j 1)]
              (if- (&&/LS (eq?/LS bidx 0) (</LS (+/LS tidx i) 0))
                   (= value 0)
                   (if- (&&/LS (eq?/LS bidx (-/LS gdimx 1))
                               (>/LS (+/LS tidx i) (-/LS bdimx 1)))
                        (= value 0)
                        (if- (&&/LS (eq?/LS bidy 0) (</LS (+/LS tidy j) 0))
                             (= value 0)
                             (if- (&&/LS (eq?/LS bidy (-/LS gdimy 1))
                                         (>/LS (+/LS tidy j) (-/LS bdimy 1)))
                                  (= value 0)
                                  (= value (synth-memory-access file i-data (+/LS g-loc i (*/LS j dataw)) 3))))))
              (+= sum (*/LS value
                            [d-kernel (+/LS KERNEL-RADIUS i)]
                            [d-kernel (+/LS KERNEL-RADIUS j)]))))
  (= [o-data g-loc] sum))



