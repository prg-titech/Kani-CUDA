#lang rosette

(require "../../lang.rkt"
         "diffusion2d-baseline.rkt"
         (rename-in rosette/lib/synthax [choose ?])
         racket/hash)

;; 9-points stencil computation example.
;; Parallel diffusion program

(define switch-w #t)
(define switch-e #f)
(define switch-n #f)
(define switch-s #f)
(define switch-nw #f)
(define switch-ne #f)
(define switch-sw #f)
(define switch-se #f)

(define (diffusion2d-kernel in
                            out
                            nx ny
                            ce cw cn cs cnw cne csw cse cc)                           
  (:= int tid-x (thread-idx 0))
  (:= int tid-y (thread-idx 1))
  (:= int BLOCKSIZE (*/LS (block-dim 0) (block-dim 1)))
  
  ; Shared memory
  (:shared real smem[BLOCKSIZE])
  
  (:= int i (+/LS (*/LS (block-dim 0) (block-idx 0)) (thread-idx 0)))
  (:= int j (+/LS (*/LS (block-dim 1) (block-idx 1)) (thread-idx 1)))
  (:= int c (+/LS (*/LS j nx) i))
  
  ; Index of a element to be updated on a shared memory
  (:= int c2 (+/LS (*/LS tid-y (block-dim 0)) tid-x))
  
  (= [smem c2] [in c])
  (? (barrier) (void))
  
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
  (:= int w (?: (eq?/LS i 0) c (-/LS c 1)))
  (:= int e (?: (eq?/LS i (-/LS nx 1)) c (+/LS c 1)))
  (:= int n (?: (eq?/LS j 0) c (-/LS c nx)))
  (:= int s (?: (eq?/LS j (-/LS ny 1)) c (+/LS c nx)))
  (:= int nw (?: (||/LS (eq?/LS i 0) (eq?/LS j 0)) c (-/LS c nx 1)))
  (:= int ne (?: (||/LS (eq?/LS i (-/LS nx 1)) (eq?/LS j 0)) c (-/LS c nx -1)))
  (:= int sw (?: (||/LS (eq?/LS i 0) (eq?/LS j (-/LS ny 1))) c (+/LS c nx -1)))
  (:= int se (?: (||/LS (eq?/LS i (-/LS nx 1)) (eq?/LS j (-/LS ny 1))) c (+/LS c nx 1)))
  (= [out c] (+/LS (*/LS cc [in c]) (*/LS cw [in w]) (*/LS ce [in e]) (*/LS cn [in n]) (*/LS cs [in s]) (*/LS cnw [in nw]) (*/LS cne [in ne]) (*/LS csw [in sw]) (*/LS cse [in se]))))

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

(define-values (SIZEX SIZEY) (values 8 8))

(define SIZE (* SIZEX SIZEY))

(define-values (BLOCKSIZEX BLOCKSIZEY) (values 4 4))

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
  (map syntax->datum (generate-forms (sat ans))))
