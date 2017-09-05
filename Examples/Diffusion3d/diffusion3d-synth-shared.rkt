#lang rosette

(require "diffusion3d-baseline.rkt"
         "../../lang.rkt"
         (rename-in rosette/lib/synthax [choose ?]))

(current-bitwidth 7)



(define (diffusion-kernel in
                          out
                          nx ny nz
                          ce cw cn cs ct cb cc)
  (:= int BLOCKSIZE (*/LS (block-dim 0) (block-dim 1)))
  
  ; Shared memory
  (:shared real smem[BLOCKSIZE])
  
  (:= int i (+/LS (*/LS (block-dim 0) (block-idx 0)) (thread-idx 0)))
  (:= int j (+/LS (*/LS (block-dim 1) (block-idx 1)) (thread-idx 1)))
  
  ; Index of a element to be updated on a global memory 
  (:= int c (+/LS (*/LS j nx) i))
  
  ; Index of a element to be updated on a shared memory
  (:= int c2 (+/LS (*/LS (thread-idx 1) (block-dim 0)) (thread-idx 0)))
  
  (:= int xy (*/LS nx ny))
  (: int tb tc tt)
  
  ; Variables for register blocking 
  (= tt [in (+/LS c xy)])
  (= tc [in c])
  (= tb tc)
  
  (? (barrier) (void))
  (= [smem c2] tc)
  (? (barrier) (void))
  
  (:= bool bw (&&/LS (eq?/LS (thread-idx 0) (? 0 (-/LS (block-dim 0) 1))) (!/LS (eq?/LS i (? 0 (-/LS nx 1))))))
  (:= bool be (&&/LS (eq?/LS (thread-idx 0) (? 0 (-/LS (block-dim 0) 1))) (!/LS (eq?/LS i (? 0 (-/LS nx 1))))))
  (:= bool bn (&&/LS (eq?/LS (thread-idx 1) (? 0 (-/LS (block-dim 1) 1))) (!/LS (eq?/LS j (? 0 (-/LS ny 1))))))
  (:= bool bs (&&/LS (eq?/LS (thread-idx 1) (? 0 (-/LS (block-dim 1) 1))) (!/LS (eq?/LS j (? 0 (-/LS ny 1))))))
  
  (:= int w (?: (eq?/LS i (? 0 ((? +/LS -/LS) (? nx ny nz) 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE))))
  (:= int e (?: (eq?/LS i (? 0 ((? +/LS -/LS) (? nx ny nz) 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE))))
  (:= int n (?: (eq?/LS j (? 0 ((? +/LS -/LS) (? nx ny nz) 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE))))
  (:= int s (?: (eq?/LS j (? 0 ((? +/LS -/LS) (? nx ny nz) 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE))))
  
  (? (barrier) (void))
  (= [out c] (+/LS (*/LS cc tc)
                   (*/LS cw (?: bw [in ((? +/LS -/LS) c 1)] [smem w]))
                   (*/LS ce (?: be [in ((? +/LS -/LS) c 1)] [smem e]))
                   (*/LS cn (?: bn [in ((? +/LS -/LS) c nx)] [smem n]))
                   (*/LS cs (?: bs [in ((? +/LS -/LS) c nx)] [smem s]))
                   (*/LS cb tb)
                   (*/LS ct tt)))
  
  (+= c xy)
  
  (:= int k 1)
  (for- [: (</LS k (-/LS nz 1)): (++ k)]
        (= tb tc)
        (= tc tt)
        
        (? (barrier) (void))
        (= [smem c2] tt)
        (= tt [in (+/LS c xy)])
        
        (? (barrier) (void))
        
        (= [out c] (+/LS (*/LS cc tc)
                         (*/LS cw (?: bw [in ((? +/LS -/LS) c 1)] [smem w]))
                         (*/LS ce (?: be [in ((? +/LS -/LS) c 1)] [smem e]))
                         (*/LS cn (?: bn [in ((? +/LS -/LS) c nx)] [smem n]))
                         (*/LS cs (?: bs [in ((? +/LS -/LS) c nx)] [smem s]))
                         (*/LS cb tb)
                         (*/LS ct tt)))
        (? (barrier) (void))
        
        (+= c xy))
  
  (= tb tc)
  (= tc tt)
  
  (? (barrier) (void))
  (= [smem c2] tt)
  
  (? (barrier) (void))
  (= [out c] (+/LS (*/LS cc tc)
                   (*/LS cw (?: bw [in ((? +/LS -/LS) c 1)] [smem w]))
                   (*/LS ce (?: be [in ((? +/LS -/LS) c 1)] [smem e]))
                   (*/LS cn (?: bn [in ((? +/LS -/LS) c nx)] [smem n]))
                   (*/LS cs (?: bs [in ((? +/LS -/LS) c nx)] [smem s]))
                   (*/LS cb tb)
                   (*/LS ct tt))))

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

(define-values (SIZEX SIZEY SIZEZ) (values 6 6 3))

(define SIZE (* SIZEX SIZEY SIZEZ))

(define-values (BLOCKSIZEX BLOCKSIZEY) (values 3 3))

;; Input array on CPU
(define CPU-in (make-array (for/vector ([i SIZE]) (make-element (r))) SIZE))
;; Input array on GPU
(define GPU-in (make-array (for/vector ([i SIZE]) (make-element (array-ref-host CPU-in i))) SIZE))
;; Output array on CPU
(define CPU-out (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
;; Output array on GPU
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
