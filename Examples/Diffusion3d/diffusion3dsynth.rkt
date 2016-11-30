#lang rosette

(require "diffusion3d.rkt"
         "../../lang.rkt")

(current-bitwidth 10)

;; Parallel diffusion program
(define (diffusion-kernel in
                          out
                          nx ny nz
                          ce cw cn cs ct cb cc)
  (:= int i (+/LS (*/LS (block-dim 0) (block-idx 0)) (thread-idx 0)))
  (:= int j (+/LS (*/LS (block-dim 1) (block-idx 1)) (thread-idx 1)))
  (:= int c (+/LS (*/LS j nx) i))
  (for ([k (in-range nz)])
    (:= int w (?: (eq?/LS i 0) c (-/LS c 1)))
    (:= int e (?: (eq?/LS i (-/LS nx 1)) c (+/LS c 1)))
    (:= int n (?: (eq?/LS j 0) c (-/LS c nx)))
    (:= int s (?: (eq?/LS j (-/LS ny 1)) c (+/LS c nx)))
    (:= int b (?: (eq?/LS k 0) c (-/LS c (??))))
    (:= int t (?: (eq?/LS k (-/LS nz 1)) c (+/LS c (??))))
    (= [out c] (+/LS (*/LS cc [in c]) (*/LS cw [in w]) (*/LS ce [in e]) (*/LS cs [in s])
                     (*/LS cn [in n]) (*/LS cb [in b]) (*/LS ct [in t])))
    (+= c (??))))

(define (diffusion-run-kernel count
                              in
                              out
                              nx ny nz
                              ce cw cn cs ct cb cc)
  (for ([i (in-range count)])
    (diffusion-kernel in
                      out
                      nx ny nz
                      ce cw cn cs ct cb cc)
    (set! in out)))

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

(define CPU-in (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
(define GPU-in (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
(define CPU-out (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
(define GPU-out (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))

(define-symbolic e w n s t b c real?)

;; Execute a diffusion program on CPU
(diffusion3d-baseline 1
                      CPU-in CPU-out
                      SIZEX SIZEY SIZEZ
                      e w n s t b c)

;; Execute a diffusion program on GPU
(invoke-kernel diffusion-run-kernel
               '(2 2)
               '(2 2)
               1
               GPU-in GPU-out
               SIZEX SIZEY SIZEZ
               e w n s t b c)


(generate-forms
 (synthesize #:forall (list e w n s t b c)
             #:guarantee (array-eq-verify CPU-out GPU-out SIZE)))