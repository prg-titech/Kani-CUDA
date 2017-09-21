#lang rosette

(require "diffusion3d-baseline.rkt"
         "../../lang.rkt"
         (rename-in rosette/lib/synthax [choose ?])
         racket/hash)

(current-bitwidth 7)

(define switch-w #t)
(define switch-e #t)
(define switch-n #t)
(define switch-s #t)

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
  
  ;(? (barrier) (void))
  (= [smem c2] tc)
  (barrier)
  ;(? (barrier) (void))
  
  ;  (define (w c)
  ;    (if switch-w
  ;        (*/LS cw (?: (&&/LS (eq?/LS tid-x (? 0 (-/LS (block-dim 0) 1))) (!/LS (eq?/LS i (? 0 (-/LS nx 1)))))
  ;                     [in ((? +/LS -/LS) c 1)]
  ;                     [smem (?: (eq?/LS i (? 0 (-/LS nx 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE)))]))
  ;        (*/LS cw (?: (eq?/LS i 0)
  ;                     [in c]
  ;                     [in (-/LS c 1)]))))
  ;  (define (e c)
  ;    (if switch-e
  ;        (*/LS ce (?: (&&/LS (eq?/LS tid-x (? 0 (-/LS (block-dim 0) 1))) (!/LS (eq?/LS i (? 0 (-/LS nx 1)))))
  ;                     [in ((? +/LS -/LS) c 1)]
  ;                     [smem (?: (eq?/LS i (? 0 (-/LS nx 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE)))]))
  ;        (*/LS ce (?: (eq?/LS i (-/LS nx 1))
  ;                     [in c]
  ;                     [in (+/LS c 1)]))))
  ;  
  ;  (define (n c)
  ;    (if switch-n
  ;        (*/LS cn (?: (&&/LS (eq?/LS tid-y (? 0 (-/LS (block-dim 1) 1))) (!/LS (eq?/LS j (? 0 (-/LS ny 1)))))
  ;                     [in ((? +/LS -/LS) c nx)]
  ;                     [smem (?: (eq?/LS j (? 0 (-/LS ny 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE)))]))
  ;        (*/LS cn (?: (eq?/LS j 0)
  ;                     [in c]
  ;                     [in (-/LS c nx)]))))
  ;  
  ;  (define (s c)
  ;    (if switch-s
  ;        (*/LS cs (?: (&&/LS (eq?/LS tid-y (? 0 (-/LS (block-dim 1) 1))) (!/LS (eq?/LS j (? 0 (-/LS ny 1)))))
  ;                     [in ((? +/LS -/LS) c nx)]
  ;                     [smem (?: (eq?/LS j (? 0 (-/LS ny 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE)))]))
  ;        (*/LS cs (?: (eq?/LS j (-/LS ny 1))
  ;                     [in c]
  ;                     [in (+/LS c nx)]))))
  
  (= [out c] (+/LS (*/LS cc tc)
                   (if switch-w
                       (*/LS cw (?: (&&/LS (eq?/LS tid-x (? 0 (-/LS (block-dim 0) 1))) (!/LS (eq?/LS i (? 0 (-/LS nx 1)))))
                                    [in ((? +/LS -/LS) c 1)]
                                    [smem (?: (eq?/LS i (? 0 (-/LS nx 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE)))]))
                       (*/LS cw (?: (eq?/LS i 0)
                                    [in c]
                                    [in (-/LS c 1)])))
                   ;(w c)
                   (if switch-e
                       (*/LS ce (?: (&&/LS (eq?/LS tid-x (? 0 (-/LS (block-dim 0) 1))) (!/LS (eq?/LS i (? 0 (-/LS nx 1)))))
                                    [in ((? +/LS -/LS) c 1)]
                                    [smem (?: (eq?/LS i (? 0 (-/LS nx 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE)))]))
                       (*/LS ce (?: (eq?/LS i (-/LS nx 1))
                                    [in c]
                                    [in (+/LS c 1)])))
                   (if switch-n
                       (*/LS cn (?: (&&/LS (eq?/LS tid-y (? 0 (-/LS (block-dim 1) 1))) (!/LS (eq?/LS j (? 0 (-/LS ny 1)))))
                                    [in ((? +/LS -/LS) c nx)]
                                    [smem (?: (eq?/LS j (? 0 (-/LS ny 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE)))]))
                       (*/LS cn (?: (eq?/LS j 0)
                                    [in c]
                                    [in (-/LS c nx)])))
                   (if switch-s
                       (*/LS cs (?: (&&/LS (eq?/LS tid-y (? 0 (-/LS (block-dim 1) 1))) (!/LS (eq?/LS j (? 0 (-/LS ny 1)))))
                                    [in ((? +/LS -/LS) c nx)]
                                    [smem (?: (eq?/LS j (? 0 (-/LS ny 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE)))]))
                       (*/LS cs (?: (eq?/LS j (-/LS ny 1))
                                    [in c]
                                    [in (+/LS c nx)])))
                   (*/LS cb tb)
                   (*/LS ct tt)))
  
  (+= c xy)
  
  (:= int k 1)
  (for- [: (</LS k (-/LS nz 1)): (++ k)]
        (= tb tc)
        (= tc tt)
        
        ;(? (barrier) (void))
        (barrier)
        (= [smem c2] tt)
        (= tt [in (+/LS c xy)])
        (barrier)
        ;(? (barrier) (void))
        
        (= [out c] (+/LS (*/LS cc tc)
                         (*/LS cw (?: (&&/LS (eq?/LS tid-x 0) (!/LS (eq?/LS i 0))) [in (-/LS c 1)] [smem (?: (eq?/LS i 0) c2 (-/LS c2 1))]))
                         (*/LS ce (?: (&&/LS (eq?/LS tid-x (-/LS (block-dim 0) 1)) (!/LS (eq?/LS i (-/LS nx 1)))) [in (+/LS c 1)] [smem (?: (eq?/LS i (-/LS nx 1)) c2 (+/LS c2 1))]))
                         (*/LS cn (?: (&&/LS (eq?/LS tid-y 0) (!/LS (eq?/LS j 0))) [in (-/LS c nx)] [smem (?: (eq?/LS j 0) c2 (-/LS c2 (block-dim 0)))]))
                         (*/LS cs (?: (&&/LS (eq?/LS tid-y (-/LS (block-dim 1) 1)) (!/LS (eq?/LS j (-/LS ny 1)))) [in (+/LS c nx)] [smem (?: (eq?/LS j (-/LS ny 1)) c2 (+/LS c2 (block-dim 0)))]))
                         (*/LS cb tb)
                         (*/LS ct tt)))
        (barrier)
        ;(? (barrier) (void))
        
        (+= c xy))
  
  (= tb tc)
  (= tc tt)
  
  ;(? (barrier) (void))
  (= [smem c2] tt)
  (barrier)
  ;(? (barrier) (void))
  
  (= [out c] (+/LS (*/LS cc tc)
                   (*/LS cw (?: (&&/LS (eq?/LS tid-x 0) (!/LS (eq?/LS i 0))) [in (-/LS c 1)] [smem (?: (eq?/LS i 0) c2 (-/LS c2 1))]))
                   (*/LS ce (?: (&&/LS (eq?/LS tid-x (-/LS (block-dim 0) 1)) (!/LS (eq?/LS i (-/LS nx 1)))) [in (+/LS c 1)] [smem (?: (eq?/LS i (-/LS nx 1)) c2 (+/LS c2 1))]))
                   (*/LS cn (?: (&&/LS (eq?/LS tid-y 0) (!/LS (eq?/LS j 0))) [in (-/LS c nx)] [smem (?: (eq?/LS j 0) c2 (-/LS c2 (block-dim 0)))]))
                   (*/LS cs (?: (&&/LS (eq?/LS tid-y (-/LS (block-dim 1) 1)) (!/LS (eq?/LS j (-/LS ny 1)))) [in (+/LS c nx)] [smem (?: (eq?/LS j (-/LS ny 1)) c2 (+/LS c2 (block-dim 0)))]))
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


(define (seq-synth-stencil)
  (set! switch-e #f)
  (set! switch-n #f)
  (set! switch-s #f)
  (define ans (model (synth-stencil)))
  (set! switch-w #f)
  (set! switch-e #t)
  (set! ans (hash-union ans (model (synth-stencil))))
  (set! switch-e #f)
  (set! switch-n #t)
  (set! ans (hash-union ans (model (synth-stencil))))
  (set! switch-n #f)
  (set! switch-s #t)
  (set! ans (hash-union ans (model (synth-stencil))))
  (set! switch-w #t)
  (set! switch-e #t)
  (set! switch-n #t)
  (set! switch-s #t)
  (map syntax->datum (generate-forms (sat ans)))1)