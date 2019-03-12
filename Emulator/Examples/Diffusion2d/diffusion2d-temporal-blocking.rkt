#lang rosette

(require "../../lang.rkt" "diffusion2d-baseline2.rkt")

(current-bitwidth #f)

(define (diffusion-kernel-temporal-blocking in out
                                            nx ny)
  (begin
    (: int i)
    (: int j)
    (: int i2)
    (: int j2)
    (: int c)
    (: int c2)
    (: int csb)
    (: int csb2))
  (=
   i
   (-/LS (+/LS (*/LS (block-idx 0) (-/LS (block-dim 0) 2)) (thread-idx 0)) 1))
  (= i (max/LS 0 i))
  (= i (min/LS i (-/LS nx 1)))
  (=
   j
   (-/LS (+/LS (*/LS (block-idx 1) (-/LS (block-dim 1) 2)) (thread-idx 1)) 1))
  (= j (max/LS 0 j))
  (= j (min/LS j (-/LS ny 1)))
  (= c (+/LS (*/LS nx j) i))
  (begin (: float (sb (*/LS (block-dim 0) (block-dim 1)))))
  (= csb (+/LS (*/LS (block-dim 0) (thread-idx 1)) (thread-idx 0)))
  (=
   (sb csb)
   (+/LS
    (+/LS (+/LS (+/LS (in (?: (eq?/LS i 0) c (-/LS c 1)))
                      (in (?: (eq?/LS i (-/LS nx 1)) c (+/LS c 1))))
                (in (?: (eq?/LS j 0) c (-/LS c nx))))
          (in (?: (eq?/LS j (-/LS ny 1)) c (+/LS c nx))))
    (in c)))
  (syncthreads)
  (=
   i2
   (+/LS
    (*/LS (block-idx 0) (-/LS (block-dim 0) 2))
    (min/LS (thread-idx 0) (-/LS (block-dim 0) 3))))
  (= i2 (min/LS i2 (-/LS nx 1)))
  (=
   j2
   (+/LS
    (*/LS (block-idx 1) (-/LS (block-dim 1) 2))
    (min/LS (thread-idx 1) (-/LS (block-dim 1) 3))))
  (= j2 (min/LS j2 (-/LS ny 1)))
  (= c2 (+/LS (*/LS nx j2) i2))
  (=
   csb2
   (+/LS
    (+/LS
     (*/LS (block-dim 0) (+/LS (modulo/LS j2 (-/LS (block-dim 1) 2)) 1))
     (modulo/LS i2 (-/LS (block-dim 0) 2)))
    1))
  (if- (&&/LS (</LS (thread-idx 0) (-/LS (block-dim 0) 2)) (</LS (thread-idx 1) (-/LS (block-dim 1) 2)))
  (=
   (out c2)
   (+/LS
    (+/LS (+/LS (+/LS (sb (?: (eq?/LS i2 (? 0 1)) csb2 (-/LS csb2 1)))
                      (sb (?: (eq?/LS i2 (-/LS nx 1)) csb2 (+/LS csb2 1))))
                (sb (?: (eq?/LS j2 0) csb2 (-/LS csb2 (block-dim 0)))))
          (sb (?: (eq?/LS i2 (-/LS ny 1)) csb2 (+/LS csb2 (block-dim 0)))))
    (sb csb2)))))




(define (diffusion-run-kernel grid
                              block
                              count
                              in out
                              nx ny)
  (for ([i (in-range count)])
    (invoke-kernel diffusion-kernel-temporal-blocking
                   grid
                   block
                   in out
                   nx ny)
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

(define-values (SIZEX SIZEY) (values 9 9))
(define SIZE (* SIZEX SIZEY))

(define-values (BLOCKSIZEX BLOCKSIZEY) (values 5 5))

(define CPU-in (make-array (for/vector ([i SIZE]) (make-element (r))) SIZE))
(define GPU-in (make-array (for/vector ([i SIZE]) (make-element (array-ref-host CPU-in i))) SIZE))
(define CPU-out (make-array (for/vector ([i SIZE]) (make-element 0)) SIZE))
(define GPU-out (make-array (for/vector ([i SIZE]) (make-element 0)) SIZE))

(define-symbolic e w n s t b c real?)

;(define (diffusion-verify) (time (verify (begin ;; Execute a diffusion program on CPU
;                                           (diffusion3d-baseline 2
;                                                                 CPU-in CPU-out
;                                                                 SIZEX SIZEY SIZEZ
;                                                                 e w n s t b c)
;                                           ;; Execute a diffusion program on GPU
;                                           (diffusion-run-kernel (list (quotient SIZEX (- BLOCKSIZEX 2)) (quotient SIZEY (- BLOCKSIZEY 2)))
;                                                                 (list BLOCKSIZEX BLOCKSIZEY)
;                                                                 1
;                                                                 GPU-in GPU-out
;                                                                 SIZEX SIZEY SIZEZ
;                                                                 e w n s t b c)
;                                           (array-eq-verify CPU-in GPU-out SIZE)))))

(define lst
  (for/list ([i SIZE])
    (array-ref-host CPU-in i)))

(time
 (begin
   (diffusion2d-baseline2 2
                          CPU-in CPU-out
                          SIZEX SIZEY)
   (diffusion-run-kernel (list (quotient SIZEX (- BLOCKSIZEX 2)) (quotient SIZEY (- BLOCKSIZEY 2)))
                         (list BLOCKSIZEX BLOCKSIZEY)
                         1
                         GPU-in GPU-out
                         SIZEX SIZEY)))



(synthesize #:forall (append lst (list e w n s t b c))
            #:guarantee (array-eq-verify
                         CPU-in GPU-out SIZE))

;(print-matrix CPU-out 1 64)
;(print-matrix GPU-out 1 64)

