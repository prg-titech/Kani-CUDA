#lang rosette

(require "../../lang.rkt" "diffusion3d-baseline.rkt")

;; 7-points stencil computation example.

;; Parallel diffusion program
(define (diffusion-kernel in
                          out
                          nx ny nz
                          ce cw cn cs ct cb cc
                          file)
  (:= int tid-x (thread-idx 0))
  (:= int tid-y (thread-idx 1))
  (:= int BLOCKSIZE (*/LS (block-dim 0) (block-dim 1)))
  (:= int c2 (+/LS (*/LS tid-y (block-dim 0)) tid-x))
  (:= int i (+/LS (*/LS (block-dim 0) (block-idx 0)) (thread-idx 0)))
  (:= int j (+/LS (*/LS (block-dim 1) (block-idx 1)) (thread-idx 1)))
  (:= int c (+/LS (*/LS j nx) i))
  (:= int xy (*/LS nx ny))
  
  (:shared real smem[BLOCKSIZE])
  (syncthreads)
  
  (for ([k (in-range nz)])
    (= [smem c2] [in c])
    (:= int w (?: (eq?/LS i 0) c (-/LS c 1)))
    (:= int e (?: (eq?/LS i (-/LS nx 1)) c (+/LS c 1)))
    (:= int n (?: (eq?/LS j 0) c (-/LS c nx)))
    (:= int s (?: (eq?/LS j (-/LS ny 1)) c (+/LS c nx)))
    (:= int b (?: (eq?/LS k 0) c (-/LS c xy)))
    (:= int t (?: (eq?/LS k (-/LS nz 1)) c (+/LS c xy)))
    (printf "w: ~a\n" w)
    (= [out c] (+/LS (*/LS cc [in c])
                     (*/LS cw (profiling-access file in w i j tid-x tid-y c2 (block-dim 0) (block-dim 1) nx ny nz))
                     (*/LS ce [in e])
                     (*/LS cs [in s])
                     (*/LS cn [in n])
                     (*/LS cb [in b])
                     (*/LS ct [in t])))
    (+= c xy)))

(define (diffusion-run-kernel file
                              grid
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
                   ce cw cn cs ct cb cc
                   file)
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

(define-values (SIZEX SIZEY SIZEZ) (values 9 9 3))
(define SIZE (* SIZEX SIZEY SIZEZ))

(define CPU-in (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
(define GPU-in (make-array (for/vector ([i SIZE]) (make-element (array-ref-host CPU-in i))) SIZE))
(define CPU-out (make-array (for/vector ([i SIZE]) (make-element 0)) SIZE))
(define GPU-out (make-array (for/vector ([i SIZE]) (make-element 0)) SIZE))

;(define-symbolic e w n s t b c real?)
(define-values (e w n s t b c) (values 1 1 1 1 1 1 1))

;; Execute a diffusion program on CPU
;(diffusion3d-baseline 2
;                      CPU-in CPU-out
;                      SIZEX SIZEY SIZEZ
;                      e w n s t b c)

;; Execute a diffusion program on GPU
(define out-file (open-output-file "profile.rkt" #:exists 'truncate))
(diffusion-run-kernel out-file
                      '(3 3)
                      '(3 3)
                      3
                      GPU-in GPU-out
                      SIZEX SIZEY SIZEZ
                      e w n s t b c)
(close-output-port out-file)


;(define (diffusion-verify) (time (verify (array-eq-verify CPU-in GPU-in SIZE))))

;(print-matrix CPU-in 8 8)
;(newline)
;(print-matrix GPU-in 8 8)
