#lang rosette

(require "diffusion3d-baseline.rkt"
         "../../lang.rkt")

(current-bitwidth 7)

(define (diffusion-kernel in
                          out
                          nx ny nz
                          ce cw cn cs ct cb cc)
  (:= int SIZE (*/LS (block-dim 0) (block-dim 1)))
  (:shared real smem[SIZE])
  (: int t1 t2 t3)
  (:= int i (+/LS (*/LS (block-dim 0) (block-idx 0)) (thread-idx 0)))
  (:= int j (+/LS (*/LS (block-dim 1) (block-idx 1)) (thread-idx 1)))
  (:= int c (+/LS (*/LS j nx) i))
  (:= int c2 (+/LS (*/LS (thread-idx 1) (block-dim 0)) (thread-idx 0)))
  (:= int xy (*/LS nx ny))
  (= [smem c2] [in c])
  (choose (barrier) (void))
  (:= bool bw (&&/LS (eq?/LS (thread-idx 0) (choose 0 (-/LS (block-dim 0) 1))) (!/LS (eq?/LS i (choose 0 (-/LS nx 1))))))
  (:= bool be (&&/LS (eq?/LS (thread-idx 0) (choose 0 (-/LS (block-dim 0) 1))) (!/LS (eq?/LS i (choose 0 (-/LS nx 1))))))
  (:= bool bn (&&/LS (eq?/LS (thread-idx 1) (choose 0 (-/LS (block-dim 1) 1))) (!/LS (eq?/LS j (choose 0 (-/LS ny 1))))))
  (:= bool bs (&&/LS (eq?/LS (thread-idx 1) (choose 0 (-/LS (block-dim 1) 1))) (!/LS (eq?/LS j (choose 0 (-/LS ny 1))))))
  ;(printf "i = ~a\n" i)
  (:= int w (?: (eq?/LS i (choose 0 (-/LS (choose nx ny nz) 1))) c2 ((choose +/LS -/LS) c2 (choose 1 (block-dim 0) SIZE))))
  (:= int e (?: (eq?/LS i (choose 0 (-/LS (choose nx ny nz) 1))) c2 ((choose +/LS -/LS) c2 (choose 1 (block-dim 0) SIZE))))
  (:= int n (?: (eq?/LS j (choose 0 (-/LS (choose nx ny nz) 1))) c2 ((choose +/LS -/LS) c2 (choose 1 (block-dim 0) SIZE))))
  (:= int s (?: (eq?/LS j (choose 0 (-/LS (choose nx ny nz) 1))) c2 ((choose +/LS -/LS) c2 (choose 1 (block-dim 0) SIZE))))
  (:= int b (choose c2 ((choose +/LS -/LS) c2 (choose 1 (block-dim 0) SIZE))))
  (:= int t (choose c2 ((choose +/LS -/LS) c2 (choose 1 (block-dim 0) SIZE))))
  (= t3 (choose [in ((choose +/LS -/LS) c xy)] [smem t]))
  (choose (barrier) (void))
  (= t2 [smem c2])
  (choose (barrier) (void))
  (= t1 (choose [in ((choose +/LS -/LS) c xy)] [smem b]))
  (choose (barrier) (void))
  ;(printf "w = ~a\n" w)
  ;; TODO (?: (cond using choose) in smem)
  (= [out c] (+/LS (*/LS cc t2)
                   (*/LS cw (?: bw [in ((choose +/LS -/LS) c 1)] [smem w]))
                   (*/LS ce (?: be [in ((choose +/LS -/LS) c 1)] [smem e]))
                   (*/LS cs (?: bs [in ((choose +/LS -/LS) c nx)] [smem s]))
                   (*/LS cn (?: bn [in ((choose +/LS -/LS) c nx)] [smem n]))
                   (*/LS cb t1)
                   (*/LS ct t3)))
  (+= c xy)
  (choose (barrier) (void))
  (:= int k 1)
  (for- [: (</LS k (-/LS nz 1)): (++ k)]
        (choose (begin (= t1 t2)
                       (= t2 t3)
                       (= [smem c2] t3)
                       (= t3 [in (+/LS c xy)]))
                (begin (= t3 t2)
                       (= t2 t1)
                       (= [smem c2] t1)
                       (= t1 [in (+/LS c xy)])))
        (choose (barrier) (void))
        ;(printf "w = ~a\n" w)
        (= [out c] (+/LS (*/LS cc t2)
                         (*/LS cw (?: bw [in ((choose +/LS -/LS) c 1)] [smem w]))
                         (*/LS ce (?: be [in ((choose +/LS -/LS) c 1)] [smem e]))
                         (*/LS cs (?: bs [in ((choose +/LS -/LS) c nx)] [smem s]))
                         (*/LS cn (?: bn [in ((choose +/LS -/LS) c nx)] [smem n]))
                         (*/LS cb t1)
                         (*/LS ct t3)))
        (+= c xy)
        (choose (barrier) (void)))

  (choose (begin (= t1 t2)
                 (= t2 t3)
                 (= [smem c2] t3)
                 (= t3 (choose t3 [in (+/LS c xy)])))
          (begin (= t3 t2)
                 (= t2 t1)
                 (= [smem c2] t1)
                 (= t1 (choose t1 [in (+/LS c xy)]))))
  (choose (barrier) (void))
  ;(printf "w = ~a\n" w)
  (= [out c] (+/LS (*/LS cc t2)
                   (*/LS cw (?: bw [in ((choose +/LS -/LS) c 1)] [smem w]))
                   (*/LS ce (?: be [in ((choose +/LS -/LS) c 1)] [smem e]))
                   (*/LS cs (?: bs [in ((choose +/LS -/LS) c nx)] [smem s]))
                   (*/LS cn (?: bn [in ((choose +/LS -/LS) c nx)] [smem n]))
                   (*/LS cb t1)
                   (*/LS ct t3))))

(define (diffusion-run-kernel grid
                              block
                              count
                              in out
                              nx ny nz
                              ce cw cn cs ct cb cc)
  (for ([i (in-range count)])
    (invoke-kernel diffusion-kernel
                   grid
                   block
                   in out
                   nx ny nz
                   ce cw cn cs ct cb cc)
    (define temp in)
    (set! in out)
    (set! out temp)))

;; Add a constraint that it is equal to each of the elements of
;; two arrays, arr1 and arr2, to asserts.
(define (array-eq-verify arr1 arr2 len)
  (for ([i (in-range len)])
    (assert
     (eq?
      (array-ref-host arr1 i)
      (array-ref-host arr2 i)))))

(define (r)
  (define-symbolic* r real?)
  r)

(define-values (SIZEX SIZEY SIZEZ) (values 3 3 5))
(define SIZE (* SIZEX SIZEY SIZEZ))

(define-values (BLOCKSIZEX BLOCKSIZEY) (values 3 3))

(define CPU-in (make-array (for/vector ([i SIZE]) (make-element (r))) SIZE))
(define GPU-in (make-array (for/vector ([i SIZE]) (make-element (array-ref-host CPU-in i))) SIZE))
(define CPU-out (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
(define GPU-out (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))

(define-symbolic e w n s t b c real?)
;(define-values (e w n s t b c) (values 1 1 1 1 1 1 1))

(define lst
  (for/list ([i SIZE])
    (array-ref-host CPU-in i)))

(define (synth-stencil)
  (time
   (synthesize #:forall (append lst (list e w n s t b c))
               #:guarantee (begin
                             ;; Execute a diffusion program on CPU
                             (diffusion3d-baseline 1
                                                   CPU-in CPU-out
                                                   SIZEX SIZEY SIZEZ
                                                   e w n s t b c)
                             
                             ;; Execute a diffusion program on GPU
                             (diffusion-run-kernel (list (quotient SIZEX BLOCKSIZEX) (quotient SIZEY BLOCKSIZEY))
                                                   (list BLOCKSIZEX BLOCKSIZEY)
                                                   1
                                                   GPU-in GPU-out
                                                   SIZEX SIZEY SIZEZ
                                                   e w n s t b c)
                             (array-eq-verify
                              CPU-out GPU-out SIZE)))))


(map syntax->datum (generate-forms (synth-stencil)))
