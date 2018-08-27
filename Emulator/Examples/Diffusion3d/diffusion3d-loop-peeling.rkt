#lang rosette

(require "../../lang.rkt" "diffusion3d-baseline.rkt")

(define (diffusion-kernel-loop-peeling in
                                       out
                                       nx ny nz
                                       ce cw cn cs ct cb cc)
  (:= int i (+/LS (*/LS (block-dim 0) (block-idx 0)) (thread-idx 0)))
  (:= int j (+/LS (*/LS (block-dim 1) (block-idx 1)) (thread-idx 1)))
  (:= int block-z (//LS nz (grid-dim 2)))
  (:= int k (*/LS block-z (block-idx 2)))
  (:= int k-end (+/LS k block-z))
  (:= int xy (*/LS nx ny))
  (:= int c (+/LS i (*/LS j nx) (*/LS k xy)))
  (:= int w (?: (eq?/LS i 0) c (-/LS c 1)))
  (:= int e (?: (eq?/LS i (-/LS nx 1)) c (+/LS c 1)))
  (:= int n (?: (eq?/LS j 0) c (-/LS c nx)))
  (:= int s (?: (eq?/LS j (-/LS ny 1)) c (+/LS c nx)))
  (: real temp-c b t1 t2)
  (= temp-c [in c])
  (= b (?: (eq?/LS k 0) temp-c [in (-/LS c xy)]))
  (= t1 [in (+/LS c xy)])
  (= t2 [in (+/LS c (*/LS 2 xy))])
  (= [out c] (+/LS (*/LS cc temp-c) (*/LS cw [in w]) (*/LS ce [in e]) (*/LS cs [in s])
                   (*/LS cn [in n]) (*/LS cb b) (*/LS ct t1)))
  (+= c xy)
  (+= w xy)
  (+= e xy)
  (+= n xy)
  (+= s xy)
  (++ k)
  (for- [: (</LS k (-/LS k-end 2)) : (++ k)]
        (= b temp-c)
        (= temp-c t1)
        (= t1 t2)
        (= t2 [in (+/LS c (*/LS xy 2))])
        (= [out c] (+/LS (*/LS cc temp-c) (*/LS cw [in w]) (*/LS ce [in e]) (*/LS cs [in s])
                         (*/LS cn [in n]) (*/LS cb b) (*/LS ct t1)))
        (+= c xy)
        (+= w xy)
        (+= e xy)
        (+= n xy)
        (+= s xy))
  (= b temp-c)
  (= temp-c t1)
  (= t1 t2)
  (= t2 (?: (</LS k (-/LS nz 2)) [in (+/LS c (*/LS xy 2))] t2))
  (= [out c] (+/LS (*/LS cc temp-c) (*/LS cw [in w]) (*/LS ce [in e]) (*/LS cs [in s])
                   (*/LS cn [in n]) (*/LS cb b) (*/LS ct t1)))
  (+= c xy)
  (+= w xy)
  (+= e xy)
  (+= n xy)
  (+= s xy)
  (++ k)
  (= b temp-c)
  (= temp-c t1)
  (= t1 t2)
  (= [out c] (+/LS (*/LS cc temp-c) (*/LS cw [in w]) (*/LS ce [in e]) (*/LS cs [in s])
                   (*/LS cn [in n]) (*/LS cb b) (*/LS ct t1))))

(define (diffusion-run-kernel count
                              in
                              out
                              nx ny nz
                              ce cw cn cs ct cb cc)
  (for ([i (in-range count)])
    (diffusion-kernel-loop-peeling in
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

(define-values (SIZEX SIZEY SIZEZ) (values 16 16 16))
(define SIZE (* SIZEX SIZEY SIZEZ))

(define CPU-in (make-array (for/vector ([i SIZE]) (make-element (r))) SIZE))
(define GPU-in (make-array (for/vector ([i SIZE]) (make-element (array-ref-host CPU-in i))) SIZE))
(define CPU-out (make-array (for/vector ([i SIZE]) (make-element 0)) SIZE))
(define GPU-out (make-array (for/vector ([i SIZE]) (make-element 0)) SIZE))

(define-symbolic e w n s t b c real?)


(define (diffusion-verify) (time (verify (begin
                                           ;; Execute a diffusion program on CPU
                                           (diffusion3d-baseline 1
                                                                 CPU-in CPU-out
                                                                 SIZEX SIZEY SIZEZ
                                                                 e w n s t b c)
                                           
                                           ;; Execute a diffusion program on GPU
                                           (invoke-kernel diffusion-run-kernel
                                                          '(4 4 4)
                                                          '(4 4)
                                                          1
                                                          GPU-in GPU-out
                                                          SIZEX SIZEY SIZEZ
                                                          e w n s t b c)(array-eq-verify CPU-out GPU-out SIZE)))))

;(print-matrix CPU-out 1 512)
;(print-matrix GPU-out 1 512)






