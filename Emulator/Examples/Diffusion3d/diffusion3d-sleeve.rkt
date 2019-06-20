#lang rosette

(require "../../lang.rkt" "diffusion3d-baseline.rkt")

;; 7-points stencil computation example.

(current-bitwidth #f)

;; Parallel diffusion program
(define (diffusion-kernel in
                          out
                          nx ny nz
                          ce cw cn cs ct cb cc)
  (:= int bx (block-dim 0))
  (:= int by (block-dim 1))
  (:= int tid-x (thread-idx 0))
  (:= int tid-y (thread-idx 1))
  (:= int BLOCKSIZE (*/LS (+/LS (block-dim 0) 2) (+/LS (block-dim 1) 2)))
  (:= int c2 (+/LS (*/LS (+/LS tid-y 1) (+/LS (block-dim 0) 2)) (+/LS tid-x 1)))
  (:= int i (+/LS (*/LS (block-dim 0) (block-idx 0)) (thread-idx 0)))
  (:= int j (+/LS (*/LS (block-dim 1) (block-idx 1)) (thread-idx 1)))
  (:= int c (+/LS (*/LS j nx) i))
  (:= int xy (*/LS nx ny))

  ;; TODO: When shared memory is declared, it's not collectly profiled
  (:shared float [smem BLOCKSIZE])
  (for- [(:= int k 0): (</LS k nz) : (++/LS k)]
        (:= int w (?: (eq?/LS i 0) c (-/LS c 1)))
        (:= int e (?: (eq?/LS i (-/LS nx 1)) c (+/LS c 1)))
        (:= int n (?: (eq?/LS j 0) c (-/LS c nx)))
        (:= int s (?: (eq?/LS j (-/LS ny 1)) c (+/LS c nx)))
        (:= int b (?: (eq?/LS k 0) c (-/LS c xy)))
        (:= int t (?: (eq?/LS k (-/LS nz 1)) c (+/LS c xy)))
    
        (= [smem c2] [in c])
        ;(if- (eq?/LS tid-y (-/LS bx 1)) (= (smem (+/LS c2 (block-dim 0) 2)) (in (?: (eq?/LS j (-/LS ny 1)) c (+/LS c nx)))))
        (if- ((? neq?/LS eq?/LS) (? i j tid-y tid-x) (? 0 (-/LS (? (block-dim 0) (block-dim 1)) (? 1 0))))
             (= (smem (+/LS c2 (block-dim 0) 2)) (in s)))
        (syncthreads)
        ;; tid bid gmix smix c i j tid-x tid-y c2 bx by nx ny nz
        (= [out c] (+/LS 
                    (*/LS cc [in c])
                    (*/LS cw [in w])
                    (*/LS ce [in e])
                    ;; in[(j == ny - 1) ? c : c + nx] => smem[c2 + blockDim.x + 2]
                    (*/LS cs [smem (+/LS c2 (block-dim 0) 2)])
                    (*/LS cn [in n])
                    (*/LS cb [in b])
                    (*/LS ct [in t])))
        (+=/LS c xy)
        (syncthreads)))

(define (diffusion-run-kernel file1
                              file2
                              file3
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
                   file1 file2
                   file3)
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

(define-values (SIZEX SIZEY SIZEZ) (values 9 6 3))
(define SIZE (* SIZEX SIZEY SIZEZ))

(define CPU-in (make-array (for/vector ([i SIZE]) (make-element (r))) SIZE))
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
(define out-file1 (open-output-file "profile1" #:exists 'truncate))
(define out-file2 (open-output-file "profile2" #:exists 'truncate))
(define out-file3 (open-output-file "profile3" #:exists 'truncate))
;(diffusion-run-kernel out-file1
;                      out-file2
;                      out-file3
;                      '(3 3)
;                      '(3 3)
;                      1
;                      GPU-in GPU-out
;                      SIZEX SIZEY SIZEZ
;                      e w n s t b c)
(close-output-port out-file1)
(close-output-port out-file2)
(close-output-port out-file3)

(define (spec-stencil)
  (begin
    ;; Execute a diffusion program on CPU
    (diffusion3d-baseline 1
                          CPU-in CPU-out
                          SIZEX SIZEY SIZEZ
                          e w n s t b c)                                           
    ;; Execute a diffusion program on GPU
    (invoke-kernel diffusion-kernel
                   '(3 3)
                   '(3 2)
                   GPU-in GPU-out
                   SIZEX SIZEY SIZEZ
                   e w n s t b c)
    (array-eq-verify CPU-out GPU-out SIZE)
    ))

(define lst
  (for/list ([i SIZE])
    (array-ref-host CPU-in i)))

(define (synth-stencil)
  (time (synthesize #:forall (append lst (list e w n s t b c))
                    #:guarantee (time 
                                 (spec-stencil)))))

(generate-forms (synth-stencil))

;(define (diffusion-verify) (time (verify (array-eq-verify CPU-in GPU-in SIZE))))

;(print-matrix CPU-in 8 8)
;(newline)
;(print-matrix GPU-in 8 8)


