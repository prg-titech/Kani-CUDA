#lang rosette

(require "../../lang.rkt" "diffusion2d-baseline2.rkt")

(current-bitwidth 10)

(define switch 0)

(define (diffusion-kernel-shared in
                                 out
                                 nx ny)
  (:= int tid-x (thread-idx 0))
  (:= int tid-y (thread-idx 1))
  (:= int i (+/LS tid-x (*/LS (block-dim 0) (block-idx 0))))
  (:= int j (+/LS tid-y (*/LS (block-dim 1) (block-idx 1))))
  (:= int xy (*/LS nx ny))
  (:shared float (sb (*/LS (block-dim 0) (block-dim 1))))
  (:= int c (+/LS i (*/LS j nx)))
  (:= int c1 (+/LS tid-x (*/LS tid-y (block-dim 0))))
  (= [sb c1] [in c])
  (syncthreads)
  (= [out c] (+/LS [sb c1]
                   (if (eq? switch 0)
                       (?: (&&/LS (eq?/LS tid-x (? 0 (-/LS (block-dim 0) 1))) ((? eq?/LS neq?/LS) i (? 0 (-/LS nx 1))))
                           [in (-/LS c 1)]
                           [sb (?: (eq?/LS i (? 0 (-/LS nx 1))) c1 (-/LS c1 (? 1 (block-dim 0))))])
                       (?: (eq?/LS i 0)
                           [in c]
                           [in (-/LS c 1)]))
                   (if (eq? switch 1)
                       (?: (&&/LS (eq?/LS tid-x (? 0 (-/LS (block-dim 0) 1))) ((? eq?/LS neq?/LS) i (? 0 (-/LS nx 1))))
                           [in (+/LS c 1)]
                           [sb (?: (eq?/LS i (? 0 (-/LS nx 1))) c1 (+/LS c1 (? 1 (block-dim 0))))])
                       (?: (eq?/LS i (-/LS nx 1))
                           [in c]
                           [in (+/LS c 1)]))
                   (if (eq? switch 2)
                       (?: (&&/LS (eq?/LS tid-y (? 0 (-/LS (block-dim 1) 1))) ((? eq?/LS neq?/LS) j (? 0 (-/LS ny 1)))) 
                           [in (-/LS c nx)]
                           [sb (?: (eq?/LS j (? 0 (-/LS ny 1))) c1 (-/LS c1 (? 1 (block-dim 1))))])
                       (?: (eq?/LS j 0)
                           [in c]
                           [in (-/LS c nx)]))
                   (if (eq? switch 3)
                       (?: (&&/LS (eq?/LS tid-y (? 0 (-/LS (block-dim 1) 1))) ((? eq?/LS neq?/LS) j (? 0 (-/LS ny 1))))
                           [in (+/LS c nx)]
                           [sb (?: (eq?/LS j (? 0 (-/LS ny 1))) c1 (+/LS c1 (? 1 (block-dim 1))))])
                       (?: (eq?/LS j (-/LS ny 1))
                           [in c]
                           [in (+/LS c nx)])))))
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

(define-values (SIZEX SIZEY) (values 6 6))
(define SIZE (* SIZEX SIZEY))

(define CPU-in (make-array (for/vector ([i SIZE]) (make-element (r))) SIZE))
(define GPU-in (make-array (for/vector ([i SIZE]) (make-element (array-ref-host CPU-in i))) SIZE))
(define CPU-out (make-array (for/vector ([i SIZE]) (make-element 0)) SIZE))
(define GPU-out (make-array (for/vector ([i SIZE]) (make-element 0)) SIZE))

(define-symbolic e w n s t b c real?)

(define lst
  (for/list ([i SIZE])
    (array-ref-host CPU-in i)))

(define (spec-stencil)
  (begin
    ;; Execute a diffusion program on CPU
    (diffusion2d-baseline2 1
                           CPU-in CPU-out
                           SIZEX SIZEY)                                           
    ;; Execute a diffusion program on GPU
    (invoke-kernel diffusion-kernel-shared
                   '(2 2)
                   '(3 3)
                   GPU-in GPU-out
                   SIZEX SIZEY)
    (array-eq-verify CPU-out GPU-out SIZE)
    ))

(define (synth-stencil)
  (time (synthesize #:forall (append lst)
                    #:guarantee (time 
                                 (spec-stencil)))))

(define (diffusion-verify) (time (verify (spec-stencil))))

(synth-stencil)