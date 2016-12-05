#lang rosette

(require "diffusion3d-baseline.rkt"
         "../../lang.rkt")

(current-bitwidth 10)

;; Parallel diffusion program
(define (diffusion-kernel in
                          out
                          nx ny nz
                          ce cw cn cs ct cb cc)
  (:= int tid-x (thread-idx 0))
  (:= int tid-y (thread-idx 1))
  (:= int i (+/LS tid-x (*/LS (block-dim 0) (block-idx 0))))
  (:= int j (+/LS tid-y (*/LS (block-dim 1) (block-idx 1))))
  (:= int xy (*/LS nx ny))
  (:shared real sb[(*/LS 4 4)])
  
  ;; The size of z dim processed by each thread
  (:= int block-z (quotient/LS nz (grid-dim 2)))
  
  ;; Start of 
  (:= int k (*/LS block-z (block-idx 2)))
  
  
  (:= int k-end (+/LS k block-z))
  
  ;; Index of global memory
  (:= int c (+/LS i (*/LS j nx) (*/LS k xy)))
  
  ;; Index of shared memory
  (:= int c1 (+/LS tid-x (*/LS tid-y (block-dim 0))))
  
  (: real t1 t2 t3)
  (= t3 [in c])
  (= t2 (?: (eq?/LS k 0) t3 [in (-/LS c xy)]))
  (:= int w (?: (eq?/LS i 0) c1 (-/LS c1 1)))
  (:= int e (?: (eq?/LS i (-/LS nx 1)) c1 (+/LS c1 1)))
  (:= int n (?: (eq?/LS j 0) c1 (-/LS c1 (block-dim 0))))
  (:= int s (?: (eq?/LS j (-/LS ny 1)) c1 (+/LS c1 (block-dim 0))))
  (:= bool bw (&&/LS (eq?/LS tid-x 0) (!/LS (eq?/LS i 0))))
  (:= bool be (&&/LS (eq?/LS tid-x (-/LS (block-dim 0) 1)) (!/LS (eq?/LS i (-/LS nx 1)))))
  (:= bool bn (&&/LS (eq?/LS tid-y 0) (!/LS (eq?/LS j 0))))
  (:= bool bs (&&/LS (eq?/LS tid-y (-/LS (block-dim 1) 1)) (!/LS (eq?/LS j (-/LS ny 1)))))
  (for- [: (</LS k (-/LS k-end 1)) : (++ k)]
        (= t1 t2)
        (= t2 t3)
        (= [sb c1] t2)
        (= t3 [in (+/LS c xy)])
        (:= real t (+/LS (*/LS cc (choose t1 t2 t3)) (*/LS cb (choose t1 t2 t3)) (*/LS ct (choose t1 t2 t3))))
        (choose (barrier) (void))
        (+= t (*/LS cw (?: bw [in (+/LS c (choose -1 0 1))] [sb w])))
        (+= t (*/LS ce (?: be [in (+/LS c (choose -1 0 1))] [sb e])))
        (+= t (*/LS cs (?: bs [in (+/LS c nx)] [sb s])))
        (+= t (*/LS cn (?: bn [in (-/LS c nx)] [sb n])))
        (choose (barrier) (void))
        (= [out c] t)
        (+= c xy))
  (= t1 t2)
  (= t2 t3)
  (choose (barrier) (void))
  (= [sb c1] t2)
  (choose (barrier) (void))
  (= t3 (?: (</LS k (+/LS nz (choose -1 0 1))) [in (+/LS c xy)] t3))
  (:= real t (+/LS (*/LS cc t2) (*/LS cb t1) (*/LS ct t3)))
  (+= t (*/LS cw (?: bw [in (+/LS c (choose -1 0 1))] [sb w])))
  (+= t (*/LS ce (?: be [in (+/LS c (choose -1 0 1))] [sb e])))
  (+= t (*/LS cs (?: bs [in ((choose -/LS +/LS) c nx)] [sb s])))
  (+= t (*/LS cn (?: bn [in ((choose -/LS +/LS) c nx)] [sb n])))
  (= [out c] t))

(define (diffusion-run-kernel
         grid
         block
         count
         in
         out
         nx ny nz
         ce cw cn cs ct cb cc)
  (for ([i (in-range count)])
    (invoke-kernel diffusion-kernel
                   grid
                   block
                   in
                   out
                   nx ny nz
                   ce cw cn cs ct cb cc)
    (set! in out))
  out)

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

(define-values (SIZEX SIZEY SIZEZ) (values 4 4 4))
(define SIZE (* SIZEX SIZEY SIZEZ))

(define CPU-in (make-array (for/vector ([i SIZE]) (make-element (r))) SIZE))
(define GPU-in (make-array (for/vector ([i SIZE]) (make-element (array-ref-host CPU-in i))) SIZE))
(define CPU-out (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
(define GPU-out (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))

(define-symbolic e w n s t b c real?)

(time
 (map syntax->datum (generate-forms
                     (synthesize #:forall (list CPU-in GPU-in e w n s t b c)
                                 #:guarantee (array-eq-verify
                                               ;; Execute a diffusion program on CPU
                                               (diffusion3d-baseline 1
                                                                     CPU-in CPU-out
                                                                     SIZEX SIZEY SIZEZ
                                                                     e w n s t b c)
                                               ;; Execute a diffusion program on GPU
                                               (diffusion-run-kernel
                                                              '(2 2 2)
                                                              '(2 2)
                                                              1
                                                              GPU-in GPU-out
                                                              SIZEX SIZEY SIZEZ
                                                              e w n s t b c)
                                                SIZE)))))