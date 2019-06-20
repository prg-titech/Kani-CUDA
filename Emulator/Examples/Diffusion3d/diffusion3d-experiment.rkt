#lang rosette

(require "diffusion3d-baseline.rkt"
         "../../lang.rkt"
         (rename-in rosette/lib/synthax [choose ?])
         racket/hash)

(current-bitwidth #f)


(define switch 0)
(define switch-seq #f)
(define switch-w #t);(or (eq? switch 0) (eq? switch-seq #f)))
(define switch-e #f);(or (eq? switch 1) (eq? switch-seq #f)))
(define switch-n #f);(or (eq? switch 2) (eq? switch-seq #f)))
(define switch-s #f);(or (eq? switch 3) (eq? switch-seq #f)))
(define switch-w1 #f);(or (eq? switch 4) (eq? switch-seq #f)))
(define switch-e1 #f);(or (eq? switch 5) (eq? switch-seq #f)))
(define switch-n1 #f);(or (eq? switch 6) (eq? switch-seq #f)))
(define switch-s1 #f);(or (eq? switch 7) (eq? switch-seq #f)))
(define switch-w2 #f);(or (eq? switch 8) (eq? switch-seq #f)))
(define switch-e2 #f);(or (eq? switch 9) (eq? switch-seq #f)))
(define switch-n2 #f);(or (eq? switch 10) (eq? switch-seq #f)))
(define switch-s2 #f);(or (eq? switch 11) (eq? switch-seq #f)))



(define (diffusion-kernel in
                          out
                          nx ny nz
                          ce cw cn cs ct cb cc)
  (:= int BLOCKSIZE (*/LS (block-dim 0) (block-dim 1)))
  
  (:= int tid-x (thread-idx 0))
  (:= int tid-y (thread-idx 1))
  
  ; Shared memory
  (:shared float [smem BLOCKSIZE])
  
  
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
  
  
  ;(? (syncthreads) (void))
  (= [smem c2] tc)
  
  (syncthreads)
  ;(? (syncthreads) (void))
  
  (:= bool bw (&&/LS (eq?/LS tid-x (? 0 (-/LS (block-dim 0) 1))) (!/LS (eq?/LS i (? 0 (-/LS nx 1))))))
  (:= bool be (&&/LS (eq?/LS tid-x (? 0 (-/LS (block-dim 0) 1))) (!/LS (eq?/LS i (? 0 (-/LS nx 1))))))
  (:= bool bn (&&/LS (eq?/LS tid-y (? 0 (-/LS (block-dim 1) 1))) (!/LS (eq?/LS j (? 0 (-/LS ny 1))))))
  (:= bool bs (&&/LS (eq?/LS tid-y (? 0 (-/LS (block-dim 1) 1))) (!/LS (eq?/LS j (? 0 (-/LS ny 1))))))
  
  
  (:= int sw (?: (eq?/LS i (? 0 ((? +/LS -/LS) (? nx ny nz) 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE))))
  (:= int se (?: (eq?/LS i (? 0 ((? +/LS -/LS) (? nx ny nz) 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE))))
  (:= int sn (?: (eq?/LS j (? 0 ((? +/LS -/LS) (? nx ny nz) 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE))))
  (:= int ss (?: (eq?/LS j (? 0 ((? +/LS -/LS) (? nx ny nz) 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE))))
  
  
  ;  (define w
  ;    (if switch-w
  ;        (*/LS cw (?: bw [in ((? +/LS -/LS) c 1)] [smem sw]))
  ;        (*/LS cw (?: (&&/LS (eq?/LS tid-x 0) (!/LS (eq?/LS i 0))) [in (-/LS c 1)] [smem (?: (eq?/LS i 0) c2 (-/LS c2 1))]))))
  ;  (define e
  ;    (if switch-e
  ;        (*/LS ce (?: be [in ((? +/LS -/LS) c 1)] [smem se]))
  ;        (*/LS ce (?: (&&/LS (eq?/LS tid-x (-/LS (block-dim 0) 1)) (!/LS (eq?/LS i (-/LS nx 1)))) [in (+/LS c 1)] [smem (?: (eq?/LS i (-/LS nx 1)) c2 (+/LS c2 1))]))))
  ;  (define n
  ;    (if switch-n
  ;        (*/LS cn (?: bn [in ((? +/LS -/LS) c nx)] [smem sn]))
  ;        (*/LS cn (?: (&&/LS (eq?/LS tid-y 0) (!/LS (eq?/LS j 0))) [in (-/LS c nx)] [smem (?: (eq?/LS j 0) c2 (-/LS c2 (block-dim 0)))]))))
  ;  (define s
  ;    (if switch-s
  ;        (*/LS cs (?: bs [in ((? +/LS -/LS) c nx)] [smem ss]))
  ;        (*/LS cs (?: (&&/LS (eq?/LS tid-y (-/LS (block-dim 1) 1)) (!/LS (eq?/LS j (-/LS ny 1)))) [in (+/LS c nx)] [smem (?: (eq?/LS j (-/LS ny 1)) c2 (+/LS c2 (block-dim 0)))]))))
  ;  (? (syncthreads) (void))
  (= [out c] (+/LS (*/LS cc tc)
                   ;(*/LS cw (?: bw [in ((? +/LS -/LS) c 1)] [smem sw]))
                   ;(*/LS ce (?: be [in ((? +/LS -/LS) c 1)] [smem se]))
                   ;(*/LS cn (?: bn [in ((? +/LS -/LS) c nx)] [smem sn]))
                   ;(*/LS cs (?: bs [in ((? +/LS -/LS) c nx)] [smem ss]))
                   (if switch-w
                       (*/LS cw (?: bw [in ((? +/LS -/LS) c (? 1 0))] [smem sw]))
                       (*/LS cw (?: (&&/LS (eq?/LS tid-x 0) (!/LS (eq?/LS i 0))) [in (-/LS c 1)] [smem (?: (eq?/LS i 0) c2 (-/LS c2 1))])))
                   (if switch-e
                       (*/LS ce (?: be [in ((? +/LS -/LS) c 1)] [smem se]))
                       (*/LS ce (?: (&&/LS (eq?/LS tid-x (-/LS (block-dim 0) 1)) (!/LS (eq?/LS i (-/LS nx 1)))) [in (+/LS c 1)] [smem (?: (eq?/LS i (-/LS nx 1)) c2 (+/LS c2 1))])))
                   (if switch-n
                       (*/LS cn (?: bn [in ((? +/LS -/LS) c nx)] [smem sn]))
                       (*/LS cn (?: (&&/LS (eq?/LS tid-y 0) (!/LS (eq?/LS j 0))) [in (-/LS c nx)] [smem (?: (eq?/LS j 0) c2 (-/LS c2 (block-dim 0)))])))
                   (if switch-s
                       (*/LS cs (?: bs [in ((? +/LS -/LS) c nx)] [smem ss]))
                       (*/LS cs (?: (&&/LS (eq?/LS tid-y (-/LS (block-dim 1) 1)) (!/LS (eq?/LS j (-/LS ny 1)))) [in (+/LS c nx)] [smem (?: (eq?/LS j (-/LS ny 1)) c2 (+/LS c2 (block-dim 0)))])))
                   (*/LS cb tb)
                   (*/LS ct tt)))
  
  (+=/LS c xy)
  
  (:= int k 1)
  (for- [: (</LS k (-/LS nz 1)): (++ k)]
        (= tb tc)
        (= tc tt)
        
        
        ;(? (syncthreads) (void))
        (syncthreads)
        (= [smem c2] tt)
        (= tt [in (+/LS c xy)])
        
        (syncthreads)
        ;(? (syncthreads) (void))
        
        (= bw (&&/LS (eq?/LS tid-x (? 0 (-/LS (block-dim 0) 1))) (!/LS (eq?/LS i (? 0 (-/LS nx 1))))))
        (= be (&&/LS (eq?/LS tid-x (? 0 (-/LS (block-dim 0) 1))) (!/LS (eq?/LS i (? 0 (-/LS nx 1))))))
        (= bn (&&/LS (eq?/LS tid-y (? 0 (-/LS (block-dim 1) 1))) (!/LS (eq?/LS j (? 0 (-/LS ny 1))))))
        (= bs (&&/LS (eq?/LS tid-y (? 0 (-/LS (block-dim 1) 1))) (!/LS (eq?/LS j (? 0 (-/LS ny 1))))))    
        
        (= sw (?: (eq?/LS i (? 0 ((? +/LS -/LS) (? nx ny nz) 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE))))
        (= se (?: (eq?/LS i (? 0 ((? +/LS -/LS) (? nx ny nz) 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE))))
        (= sn (?: (eq?/LS j (? 0 ((? +/LS -/LS) (? nx ny nz) 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE))))
        (= ss (?: (eq?/LS j (? 0 ((? +/LS -/LS) (? nx ny nz) 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE))))
        
        (= [out c] (+/LS (*/LS cc tc)
                         ;(*/LS cw (?: bw [in ((? +/LS -/LS) c 1)] [smem sw]))
                         ;(*/LS ce (?: be [in ((? +/LS -/LS) c 1)] [smem se]))
                         ;(*/LS cn (?: bn [in ((? +/LS -/LS) c nx)] [smem sn]))
                         ;(*/LS cs (?: bs [in ((? +/LS -/LS) c nx)] [smem ss]))
                         (if switch-w1
                             (*/LS cw (?: bw [in ((? +/LS -/LS) c 1)] [smem sw]))
                             (*/LS cw (?: (&&/LS (eq?/LS tid-x 0) (!/LS (eq?/LS i 0))) [in (-/LS c 1)] [smem (?: (eq?/LS i 0) c2 (-/LS c2 1))])))
                         (if switch-e1
                             (*/LS ce (?: be [in ((? +/LS -/LS) c 1)] [smem se]))
                             (*/LS ce (?: (&&/LS (eq?/LS tid-x (-/LS (block-dim 0) 1)) (!/LS (eq?/LS i (-/LS nx 1)))) [in (+/LS c 1)] [smem (?: (eq?/LS i (-/LS nx 1)) c2 (+/LS c2 1))])))
                         (if switch-n1
                             (*/LS cn (?: bn [in ((? +/LS -/LS) c nx)] [smem sn]))
                             (*/LS cn (?: (&&/LS (eq?/LS tid-y 0) (!/LS (eq?/LS j 0))) [in (-/LS c nx)] [smem (?: (eq?/LS j 0) c2 (-/LS c2 (block-dim 0)))])))
                         (if switch-s1
                             (*/LS cs (?: bs [in ((? +/LS -/LS) c nx)] [smem ss]))
                             (*/LS cs (?: (&&/LS (eq?/LS tid-y (-/LS (block-dim 1) 1)) (!/LS (eq?/LS j (-/LS ny 1)))) [in (+/LS c nx)] [smem (?: (eq?/LS j (-/LS ny 1)) c2 (+/LS c2 (block-dim 0)))])))
                         (*/LS cb tb)
                         (*/LS ct tt)))
        
        (syncthreads)
        ;(? (syncthreads) (void))
        
        (+=/LS c xy))
  
  (= tb tc)
  (= tc tt) 
  
  ;(? (syncthreads) (void))
  (= [smem c2] tt)
  
  (syncthreads)
  ;(? (syncthreads) (void))
  (= bw (&&/LS (eq?/LS tid-x (? 0 (-/LS (block-dim 0) 1))) (!/LS (eq?/LS i (? 0 (-/LS nx 1))))))
  (= be (&&/LS (eq?/LS tid-x (? 0 (-/LS (block-dim 0) 1))) (!/LS (eq?/LS i (? 0 (-/LS nx 1))))))
  (= bn (&&/LS (eq?/LS tid-y (? 0 (-/LS (block-dim 1) 1))) (!/LS (eq?/LS j (? 0 (-/LS ny 1))))))
  (= bs (&&/LS (eq?/LS tid-y (? 0 (-/LS (block-dim 1) 1))) (!/LS (eq?/LS j (? 0 (-/LS ny 1))))))  
  
  (= sw (?: (eq?/LS i (? 0 ((? +/LS -/LS) (? nx ny nz) 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE))))
  (= se (?: (eq?/LS i (? 0 ((? +/LS -/LS) (? nx ny nz) 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE))))
  (= sn (?: (eq?/LS j (? 0 ((? +/LS -/LS) (? nx ny nz) 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE))))
  (= ss (?: (eq?/LS j (? 0 ((? +/LS -/LS) (? nx ny nz) 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE))))
  
  (= [out c] (+/LS (*/LS cc tc)
                   ;(*/LS cw (?: bw [in ((? +/LS -/LS) c 1)] [smem sw]))
                   ;(*/LS ce (?: be [in ((? +/LS -/LS) c 1)] [smem se]))
                   ;(*/LS cn (?: bn [in ((? +/LS -/LS) c nx)] [smem sn]))
                   ;(*/LS cs (?: bs [in ((? +/LS -/LS) c nx)] [smem ss]))
                   (if switch-w2
                       (*/LS cw (?: bw [in ((? +/LS -/LS) c 1)] [smem sw]))
                       (*/LS cw (?: (&&/LS (eq?/LS tid-x 0) (!/LS (eq?/LS i 0))) [in (-/LS c 1)] [smem (?: (eq?/LS i 0) c2 (-/LS c2 1))])))
                   (if switch-e2
                       (*/LS ce (?: be [in ((? +/LS -/LS) c 1)] [smem se]))
                       (*/LS ce (?: (&&/LS (eq?/LS tid-x (-/LS (block-dim 0) 1)) (!/LS (eq?/LS i (-/LS nx 1)))) [in (+/LS c 1)] [smem (?: (eq?/LS i (-/LS nx 1)) c2 (+/LS c2 1))])))
                   (if switch-n2
                       (*/LS cn (?: bn [in ((? +/LS -/LS) c nx)] [smem sn]))
                       (*/LS cn (?: (&&/LS (eq?/LS tid-y 0) (!/LS (eq?/LS j 0))) [in (-/LS c nx)] [smem (?: (eq?/LS j 0) c2 (-/LS c2 (block-dim 0)))])))
                   (if switch-s2
                       (*/LS cs (?: bs [in ((? +/LS -/LS) c nx)] [smem ss]))
                       (*/LS cs (?: (&&/LS (eq?/LS tid-y (-/LS (block-dim 1) 1)) (!/LS (eq?/LS j (-/LS ny 1)))) [in (+/LS c nx)] [smem (?: (eq?/LS j (-/LS ny 1)) c2 (+/LS c2 (block-dim 0)))])))
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

(define-values (SIZEX SIZEY SIZEZ) (values 12 9 3))

(define SIZE (* SIZEX SIZEY SIZEZ))

(define-values (BLOCKSIZEX BLOCKSIZEY) (values 4 3))

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
                             (time 
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
                               CPU-out GPU-out SIZE))))))
                             ;(println (length (asserts)))))))
;;409
;;484
;;599
;;764
;;836
;;908
;;1016
;;1160
;;1233
;;1308
;;1387


;(map syntax->datum (generate-forms (synth-stencil)))

(define (seq-synth-stencil n)
  (time
   (define ans (model (synth-stencil)))
   (for ([i (- n 1)])
     (set! switch (+ i 1))
     (set! ans (hash-union ans (model (synth-stencil))
                           #:combine/key (lambda (k v1 v2) (and v1 v2))))
     )
   (map syntax->datum (generate-forms (sat ans)))))

(generate-forms (synth-stencil))
;(seq-synth-stencil 4)