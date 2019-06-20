#lang rosette

(require "../../lang.rkt"
         "diffusion2d-baseline.rkt"
         (rename-in rosette/lib/synthax [choose ?])
         racket/hash)

;; 9-points stencil computation example.
;; Parallel diffusion program

(define switch 0)
(define switch-seq #f)
(define switch-w (or (eq? switch 0) (eq? switch-seq #f)))
(define switch-e (or (eq? switch 1) (eq? switch-seq #f)))
(define switch-n (or (eq? switch 2) (eq? switch-seq #f)))
(define switch-s (or (eq? switch 3) (eq? switch-seq #f)))
(define switch-nw (or (eq? switch 4) (eq? switch-seq #f)))
(define switch-ne (or (eq? switch 5) (eq? switch-seq #f)))
(define switch-sw (or (eq? switch 6) (eq? switch-seq #f)))
(define switch-se (or (eq? switch 7) (eq? switch-seq #f)))
(current-bitwidth 7)

(define (diffusion2d-kernel in
                            out
                            nx ny
                            ce cw cn cs cnw cne csw cse cc)                           
  (:= int tid-x (thread-idx 0))
  (:= int tid-y (thread-idx 1))
  (:= int BLOCKSIZE (*/LS (block-dim 0) (block-dim 1)))
  
  ; Shared memory
  (:shared float [smem BLOCKSIZE])
  
  (:= int i (+/LS (*/LS (block-dim 0) (block-idx 0)) (thread-idx 0)))
  (:= int j (+/LS (*/LS (block-dim 1) (block-idx 1)) (thread-idx 1)))
  (:= int c (+/LS (*/LS j nx) i))
  
  ; Index of a element to be updated on a shared memory
  (:= int c2 (+/LS (*/LS tid-y (block-dim 0)) tid-x))
  
  (= [smem c2] [in c])
  ;(? (syncthreads) (void))
  (syncthreads)
  
  ;  (:= int w (if switch-w
  ;                (*/LS cw (?: (&&/LS (eq?/LS tid-x (? 0 (-/LS (block-dim 0) 1))) (!/LS (eq?/LS i (? 0 (-/LS nx 1)))))
  ;                             [in ((? +/LS -/LS) c 1)]
  ;                             [smem (?: (eq?/LS i (? 0 (-/LS nx 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE)))]))
  ;                (*/LS cw (?: (eq?/LS i 0)
  ;                             [in c]
  ;                             [in (-/LS c 1)]))))
  ;  (:= int e (if switch-e
  ;                (*/LS ce (?: (&&/LS (eq?/LS tid-x (? 0 (-/LS (block-dim 0) 1))) (!/LS (eq?/LS i (? 0 (-/LS nx 1)))))
  ;                             [in ((? +/LS -/LS) c 1)]
  ;                             [smem (?: (eq?/LS i (? 0 (-/LS nx 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE)))]))
  ;                (*/LS ce (?: (eq?/LS i (-/LS nx 1))
  ;                             [in c]
  ;                             [in (+/LS c 1)]))))
  ;  (:= int n (if switch-n
  ;                (*/LS cn (?: (&&/LS (eq?/LS tid-y (? 0 (-/LS (block-dim 1) 1))) (!/LS (eq?/LS j (? 0 (-/LS ny 1)))))
  ;                             [in ((? +/LS -/LS) c nx)]
  ;                             [smem (?: (eq?/LS j (? 0 (-/LS ny 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE)))]))
  ;                (*/LS cn (?: (eq?/LS j 0)
  ;                             [in c]
  ;                             [in (-/LS c nx)]))))
  ;  (:= int s (if switch-s
  ;                (*/LS cs (?: (&&/LS (eq?/LS tid-y (? 0 (-/LS (block-dim 1) 1))) (!/LS (eq?/LS j (? 0 (-/LS ny 1)))))
  ;                             [in ((? +/LS -/LS) c nx)]
  ;                             [smem (?: (eq?/LS j (? 0 (-/LS ny 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE)))]))
  ;                (*/LS cs (?: (eq?/LS j (-/LS ny 1))
  ;                             [in c]
  ;                             [in (+/LS c nx)]))))
  (:= bool bw (&&/LS (eq?/LS tid-x (? 0 (-/LS (block-dim 0) 1))) (!/LS (eq?/LS i (? 0 (-/LS nx 1))))))
  (:= bool be (&&/LS (eq?/LS tid-x (? 0 (-/LS (block-dim 0) 1))) (!/LS (eq?/LS i (? 0 (-/LS nx 1))))))
  (:= bool bn (&&/LS (eq?/LS tid-y (? 0 (-/LS (block-dim 1) 1))) (!/LS (eq?/LS j (? 0 (-/LS ny 1))))))
  (:= bool bs (&&/LS (eq?/LS tid-y (? 0 (-/LS (block-dim 1) 1))) (!/LS (eq?/LS j (? 0 (-/LS ny 1))))))
  (:= bool bnw (||/LS (&&/LS (eq?/LS tid-y (? 0 (-/LS (block-dim 1) 1))) (!/LS (eq?/LS j (? 0 (-/LS ny 1)))))
                      (&&/LS (eq?/LS tid-x (? 0 (-/LS (block-dim 0) 1))) (!/LS (eq?/LS i (? 0 (-/LS nx 1)))))))
  (:= bool bne (||/LS (&&/LS (eq?/LS tid-y (? 0 (-/LS (block-dim 1) 1))) (!/LS (eq?/LS j (? 0 (-/LS ny 1)))))
                      (&&/LS (eq?/LS tid-x (? 0 (-/LS (block-dim 0) 1))) (!/LS (eq?/LS i (? 0 (-/LS nx 1)))))))
  (:= bool bsw (||/LS (&&/LS (eq?/LS tid-y (? 0 (-/LS (block-dim 1) 1))) (!/LS (eq?/LS j (? 0 (-/LS ny 1)))))
                      (&&/LS (eq?/LS tid-x (? 0 (-/LS (block-dim 0) 1))) (!/LS (eq?/LS i (? 0 (-/LS nx 1)))))))
  (:= bool bse (||/LS (&&/LS (eq?/LS tid-y (? 0 (-/LS (block-dim 1) 1))) (!/LS (eq?/LS j (? 0 (-/LS ny 1)))))
                      (&&/LS (eq?/LS tid-x (? 0 (-/LS (block-dim 0) 1))) (!/LS (eq?/LS i (? 0 (-/LS nx 1)))))))
  
  (:= int s-w (?: (eq?/LS i (? 0 ((? +/LS -/LS) (? nx ny) 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE))))
  (:= int s-e (?: (eq?/LS i (? 0 ((? +/LS -/LS) (? nx ny) 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE))))
  (:= int s-n (?: (eq?/LS j (? 0 ((? +/LS -/LS) (? nx ny) 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE))))
  (:= int s-s (?: (eq?/LS j (? 0 ((? +/LS -/LS) (? nx ny) 1))) c2 ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE))))
  
  (:= int s-nw (?: (eq?/LS i (? 0 ((? +/LS -/LS) (? nx ny) 1))) c2 ((? +/LS -/LS) ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE)) 1)))
  (:= int s-ne (?: (eq?/LS i (? 0 ((? +/LS -/LS) (? nx ny) 1))) c2 ((? +/LS -/LS) ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE)) 1)))
  (:= int s-sw (?: (eq?/LS j (? 0 ((? +/LS -/LS) (? nx ny) 1))) c2 ((? +/LS -/LS) ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE)) 1)))
  (:= int s-se (?: (eq?/LS j (? 0 ((? +/LS -/LS) (? nx ny) 1))) c2 ((? +/LS -/LS) ((? +/LS -/LS) c2 (? 1 (block-dim 0) SIZE)) 1)))
  
  (:= int w (?: (eq?/LS i 0) c (-/LS c 1)))
  (:= int e (?: (eq?/LS i (-/LS nx 1)) c (+/LS c 1)))
  (:= int n (?: (eq?/LS j 0) c (-/LS c nx)))
  (:= int s (?: (eq?/LS j (-/LS ny 1)) c (+/LS c nx)))
  
  (:= int nw (?: (||/LS (eq?/LS i 0) (eq?/LS j 0)) c (-/LS c nx 1)))
  (:= int ne (?: (||/LS (eq?/LS i (-/LS nx 1)) (eq?/LS j 0)) c (-/LS c nx -1)))
  (:= int sw (?: (||/LS (eq?/LS i 0) (eq?/LS j (-/LS ny 1))) c (+/LS c nx -1)))
  (:= int se (?: (||/LS (eq?/LS i (-/LS nx 1)) (eq?/LS j (-/LS ny 1))) c (+/LS c nx 1)))
  
  
  (= [out c] (+/LS (*/LS cc [in c])
                   (*/LS cw (if switch-w
                                (?: bw [in w] [smem s-w])
                                (?: (eq?/LS i 0) [in c] [in (-/LS c 1)])))
                   (*/LS ce (if switch-e
                                (?: be [in e] [smem s-e])
                                (?: (eq?/LS i (-/LS nx 1)) [in c] [in (+/LS c 1)])))
                   (*/LS cn (if switch-n
                                (?: bn [in n] [smem s-n])
                                (?: (eq?/LS j 0) [in c] [in (-/LS c nx)])))
                   (*/LS cs (if switch-s
                                (?: bs [in s] [smem s-s])
                                (?: (eq?/LS j (-/LS ny 1)) [in c] [in (+/LS c nx)])))
                   (*/LS cnw (if switch-nw
                                 (?: (||/LS bn bw) [in nw] [smem s-nw])
                                 [in nw]))
                   (*/LS cne (if switch-ne
                                 (?: (||/LS bn be) [in ne] [smem s-ne])
                                 [in ne]))
                   (*/LS csw (if switch-sw
                                 (?: (||/LS bs bw) [in sw] [smem s-sw])
                                 [in sw]))
                   (*/LS cse (if switch-se
                                 (?: (||/LS bs be) [in se] [smem s-se])
                                 [in se])))))
;                   (*/LS cw [in w])
;                   (*/LS ce [in e])
;                   (*/LS cn [in n])
;                   (*/LS cs [in s])
;                   (*/LS cnw [in nw])
;                   (*/LS cne [in ne])
;                   (*/LS csw [in sw])
;                   (*/LS cse [in se]))))

(define (diffusion2d-run-kernel grid
                                block
                                count
                                in out
                                nx ny
                                ce cw cn cs cnw cne csw cse cc)
  (for ([i (in-range count)])
    (invoke-kernel diffusion2d-kernel
                   grid
                   block
                   in out
                   nx ny
                   ce cw cn cs cnw cne csw cse cc)
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

(define-values (SIZEX SIZEY) (values 12 9))

(define SIZE (* SIZEX SIZEY))

(define-values (BLOCKSIZEX BLOCKSIZEY) (values 4 3))

(define CPU-in (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
(define GPU-in (make-array (for/vector ([i SIZE]) (make-element (array-ref-host CPU-in i))) SIZE))
(define CPU-out (make-array (for/vector ([i SIZE]) (make-element 0)) SIZE))
(define GPU-out (make-array (for/vector ([i SIZE]) (make-element 0)) SIZE))

(define-symbolic e w n s nw ne sw se c real?)

(define lst
  (for/list ([i SIZE])
    (array-ref-host CPU-in i)))

(define (synth-stencil)
  (time
   (synthesize #:forall (append lst (list e w n s nw ne sw se c))
               #:guarantee (begin
                             ;; Execute a diffusion program on CPU
                             (diffusion2d-baseline 1
                                                   CPU-in CPU-out
                                                   SIZEX SIZEY
                                                   e w n s nw ne sw se c)
                             ;; Execute a diffusion program on GPU
                             (diffusion2d-run-kernel (list (quotient SIZEX BLOCKSIZEX) (quotient SIZEY BLOCKSIZEY))
                                                     (list BLOCKSIZEX BLOCKSIZEY)
                                                     1
                                                     GPU-in GPU-out
                                                     SIZEX SIZEY
                                                     e w n s nw ne sw se c)
                             (array-eq-verify
                              CPU-out GPU-out SIZE)))))


(define (seq-synth-stencil n)
  (time
   (set! switch 0)
   (define ans (model (synth-stencil)))
   (for ([i (in-range 1 n)])
     (set! switch i)
     (set! ans (hash-union ans (model (synth-stencil))
                           #:combine/key (lambda (k v1 v2) (and v1 v2))))
     )
   (set! switch -1)
   (map syntax->datum (generate-forms (sat ans)))))

(seq-synth-stencil 1)
