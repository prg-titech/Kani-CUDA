#lang rosette

(require "../../lang.rkt" "diffusion2d-baseline.rkt")

;; 9-points stencil computation example.

;; Parallel diffusion program
(define (diffusion2d-kernel in
                            out
                            nx ny
                            ce cw cn cs cnw cne csw cse cc)
  (:= int i (+/LS (*/LS (block-dim 0) (block-idx 0)) (thread-idx 0)))
  (:= int j (+/LS (*/LS (block-dim 1) (block-idx 1)) (thread-idx 1)))
  (:= int c (+/LS (*/LS j nx) i))
  (:= int w (?: (eq?/LS i 0) c (-/LS c 1)))
  (:= int e (?: (eq?/LS i (-/LS nx 1)) c (+/LS c 1)))
  (:= int n (?: (eq?/LS j 0) c (-/LS c nx)))
  (:= int s (?: (eq?/LS j (-/LS ny 1)) c (+/LS c nx)))
  (:= int nw (?: (||/LS (eq?/LS i 0) (eq?/LS j 0)) c (-/LS c nx 1)))
  (:= int ne (?: (||/LS (eq?/LS i (-/LS nx 1)) (eq?/LS j 0)) c (-/LS c nx -1)))
  (:= int sw (?: (||/LS (eq?/LS i 0) (eq?/LS j (-/LS ny 1))) c (+/LS c nx -1)))
  (:= int se (?: (||/LS (eq?/LS i (-/LS nx 1)) (eq?/LS j (-/LS ny 1))) c (+/LS c nx 1)))
  (= [out c] (+/LS (*/LS cc [in c]) (*/LS cw [in w]) (*/LS ce [in e]) (*/LS cs [in s])
                   (*/LS cn [in n]) (*/LS cnw [in nw]) (*/LS cne [in ne]) (*/LS csw [in sw]) (*/LS cse [in se]))))

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

(define CPU-in (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
(define GPU-in (make-array (for/vector ([i SIZE]) (make-element (array-ref-host CPU-in i))) SIZE))
(define CPU-out (make-array (for/vector ([i SIZE]) (make-element 0)) SIZE))
(define GPU-out (make-array (for/vector ([i SIZE]) (make-element 0)) SIZE))

(define-symbolic e w n s nw ne sw se c real?)

;; Execute a diffusion program on CPU
(diffusion2d-baseline 1
                      CPU-in CPU-out
                      SIZEX SIZEY
                      e w n s nw ne sw se c)

;; Execute a diffusion program on GPU
(time (diffusion2d-run-kernel
       '(2 2)
       '(4 4)
       1
       GPU-in GPU-out
       SIZEX SIZEY
       e w n s nw ne sw se c))


(define (diffusion-verify) (time (verify (array-eq-verify CPU-out GPU-out SIZE))))

;(print-matrix CPU-in 8 8)
;(newline)
;(print-matrix GPU-in 8 8)
