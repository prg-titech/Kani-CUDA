#lang rosette

(require "diffusion3d-baseline.rkt"
         "../../lang.rkt")

(current-bitwidth 7)

(define (diffusion-kernel in
                          out
                          nx ny nz
                          ce cw cn cs ct cb cc)
  (:= int i (+/LS (*/LS (block-dim 0) (block-idx 0)) (thread-idx 0)))
  (:= int j (+/LS (*/LS (block-dim 1) (block-idx 1)) (thread-idx 1)))
  (:= int c (+/LS (*/LS j nx) i))
  (:= int xy (*/LS nx ny))
  ;(printf "i = ~a\n" i)
  (:= int w (?: (eq?/LS i (choose 0 (-/LS (choose nx ny nz) 1))) c ((choose +/LS -/LS) c (choose 1 nx xy))))
  (:= int e (?: (eq?/LS i (choose 0 (-/LS (choose nx ny nz) 1))) c ((choose +/LS -/LS) c (choose 1 nx xy))))
  (:= int n (?: (eq?/LS j (choose 0 (-/LS (choose nx ny nz) 1))) c ((choose +/LS -/LS) c (choose 1 nx xy))))
  (:= int s (?: (eq?/LS j (choose 0 (-/LS (choose nx ny nz) 1))) c ((choose +/LS -/LS) c (choose 1 nx xy))))
  (:= int b (choose c ((choose +/LS -/LS) c (choose 1 nx xy))))
  (:= int t (choose c ((choose +/LS -/LS) c (choose 1 nx xy))))
  ;(printf "w = ~a\n" w)
  (= [out c] (+/LS (*/LS cc [in c]) (*/LS cw [in w]) (*/LS ce [in e]) (*/LS cs [in s])
                   (*/LS cn [in n]) (*/LS cb [in b]) (*/LS ct [in t])))
  (+= c xy)
  (choose (barrier) (void))
  (:= int k 1)
  (for- [: (</LS k (-/LS nz 1)): (++ k)]
        (= w (?: (eq?/LS i (choose 0 (-/LS (choose nx ny nz) 1))) c ((choose +/LS -/LS) c (choose 1 nx xy))))
        (= e (?: (eq?/LS i (choose 0 (-/LS (choose nx ny nz) 1))) c ((choose +/LS -/LS) c (choose 1 nx xy))))
        (= n (?: (eq?/LS j (choose 0 (-/LS (choose nx ny nz) 1))) c ((choose +/LS -/LS) c (choose 1 nx xy))))
        (= s (?: (eq?/LS j (choose 0 (-/LS (choose nx ny nz) 1))) c ((choose +/LS -/LS) c (choose 1 nx xy))))
        (= b ((choose +/LS -/LS) c (choose 1 nx xy)))
        (= t ((choose +/LS -/LS) c (choose 1 nx xy)))
        ;(printf "w = ~a\n" w)
        (= [out c] (+/LS (*/LS cc [in c]) (*/LS cw [in w]) (*/LS ce [in e]) (*/LS cs [in s])
                         (*/LS cn [in n]) (*/LS cb [in b]) (*/LS ct [in t])))
        (+= c xy))
  (choose (barrier) (void))
  (= w (?: (eq?/LS i (choose 0 (-/LS (choose nx ny nz) 1))) c ((choose +/LS -/LS) c (choose 1 nx xy))))
  (= e (?: (eq?/LS i (choose 0 (-/LS (choose nx ny nz) 1))) c ((choose +/LS -/LS) c (choose 1 nx xy))))
  (= n (?: (eq?/LS j (choose 0 (-/LS (choose nx ny nz) 1))) c ((choose +/LS -/LS) c (choose 1 nx xy))))
  (= s (?: (eq?/LS j (choose 0 (-/LS (choose nx ny nz) 1))) c ((choose +/LS -/LS) c (choose 1 nx xy))))
  (= b (choose c ((choose +/LS -/LS) c (choose 1 nx xy))))
  (= t (choose c ((choose +/LS -/LS) c (choose 1 nx xy))))
  ;(printf "w = ~a\n" w)
  (= [out c] (+/LS (*/LS cc [in c]) (*/LS cw [in w]) (*/LS ce [in e]) (*/LS cs [in s])
                   (*/LS cn [in n]) (*/LS cb [in b]) (*/LS ct [in t]))))

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

(define-values (SIZEX SIZEY SIZEZ) (values 3 3 3))
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
