#lang rosette
(require "lang.rkt")

(current-bitwidth 7)

(define (diffusion-kernel in out nx ny nz ce cw cn cs ct cb cc)
  (:= int BLOCKSIZE (*/LS (block-dim 0) (block-dim 1)))
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (:= int tid-x (thread-idx 0))
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (:= int tid-y (thread-idx 1))
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (:shared real smem (BLOCKSIZE))
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (:= int i (+/LS (*/LS (block-dim 0) (block-idx 0)) tid-x))
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (:= int j (+/LS (*/LS (block-dim 1) (block-idx 1)) tid-y))
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (:= int c (+/LS (*/LS j nx) i))
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (:= int c2 (+/LS (*/LS tid-y (block-dim 0)) tid-x))
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (:= int xy (*/LS nx ny))
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (: int tb tc tt)
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (= tt (in (+/LS c xy)))
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (= tc (in c))
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (= tb tc)
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (= (smem c2) tc)
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (:=
   bool
   bw
   (&&/LS
    (eq?/LS tid-x (? 0 (-/LS (block-dim 0) 1)))
    (!/LS (eq?/LS i (? 0 (-/LS nx 1))))))
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (:=
   bool
   be
   (&&/LS
    (eq?/LS tid-x (? 0 (-/LS (block-dim 0) 1)))
    (!/LS (eq?/LS i (? 0 (-/LS nx 1))))))
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (:=
   bool
   bn
   (&&/LS
    (eq?/LS tid-y (? 0 (-/LS (block-dim 1) 1)))
    (!/LS (eq?/LS j (? 0 (-/LS ny 1))))))
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (:=
   bool
   bs
   (&&/LS
    (eq?/LS tid-y (? 0 (-/LS (block-dim 1) 1)))
    (!/LS (eq?/LS j (? 0 (-/LS ny 1))))))
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (:=
   int
   sw
   (?: (eq?/LS i (? 0 (-/LS nx 1))) c2 (-/LS c2 (? 1 (block-dim 0)))))
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (:=
   int
   se
   (?: (eq?/LS i (? 0 (-/LS nx 1))) c2 (+/LS c2 (? 1 (block-dim 0)))))
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (:=
   int
   sn
   (?: (eq?/LS j (? 0 (-/LS ny 1))) c2 (-/LS c2 (? 1 (block-dim 0)))))
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (:=
   int
   ss
   (?: (eq?/LS j (? 0 (-/LS ny 1))) c2 (+/LS c2 (? 1 (block-dim 0)))))
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (=
   (out c)
   (+/LS
    (*/LS cc tc)
    (*/LS cw (?: bw (in (-/LS c 1)) (smem sw)))
    (*/LS ce (?: be (in (+/LS c 1)) (smem se)))
    (*/LS cn (?: bn (in (-/LS c nx)) (smem sn)))
    (*/LS cs (?: bs (in (+/LS c nx)) (smem ss)))
    (*/LS cb tb)
    (*/LS ct tt)))
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (+= c xy)
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (:= int k 1)
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (for-
   (: (</LS k (-/LS nz 1)) : (++ k))
   (= tb tc)
   (if (switch) (? (syncthreads) (void)) (syncthreads))
   (= tc tt)
   (if (switch) (? (syncthreads) (void)) (syncthreads))
   (= (smem c2) tt)
   (if (switch) (? (syncthreads) (void)) (syncthreads))
   (= tt (in (+/LS c xy)))
   (if (switch) (? (syncthreads) (void)) (syncthreads))
   (=
    (out c)
    (+/LS
     (*/LS cc tc)
     (*/LS cw (?: bw (in (-/LS c 1)) (smem sw)))
     (*/LS ce (?: be (in (+/LS c 1)) (smem se)))
     (*/LS cn (?: bn (in (-/LS c nx)) (smem sn)))
     (*/LS cs (?: bs (in (+/LS c nx)) (smem ss)))
     (*/LS cb tb)
     (*/LS ct tt)))
   (if (switch) (? (syncthreads) (void)) (syncthreads))
   (+= c xy))
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (= tb tc)
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (= tc tt)
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (= (smem c2) tt)
  (if (switch) (? (syncthreads) (void)) (syncthreads))
  (=
   (out c)
   (+/LS
    (*/LS cc tc)
    (*/LS cw (?: bw (in (-/LS c 1)) (smem sw)))
    (*/LS ce (?: be (in (+/LS c 1)) (smem se)))
    (*/LS cn (?: bn (in (-/LS c nx)) (smem sn)))
    (*/LS cs (?: bs (in (+/LS c nx)) (smem ss)))
    (*/LS cb tb)
    (*/LS ct tt))))

(define (diffusion-run-kernel
         grid
         block
         count
         in
         out
         nx
         ny
         nz
         ce
         cw
         cn
         cs
         ct
         cb
         cc)
  (for
   ((i (in-range count)))
   (invoke-kernel
    diffusion-kernel
    grid
    block
    in
    out
    nx
    ny
    nz
    ce
    cw
    cn
    cs
    ct
    cb
    cc)
   (define temp in)
   (set! in out)
   (set! out temp)))

(define (diffusion3d-baseline-iteration in out nx ny nz ce cw cn cs ct cb cc)
  (for
   ((z (in-range nz)))
   (for
    ((y (in-range ny)))
    (for
     ((x (in-range nx)))
     (define-values (c w e n s b t) (values 0 0 0 0 0 0 0))
     (set! c (+ x (* y nx) (* z nx ny)))
     (set! w (if (eq? x 0) c (- c 1)))
     (set! e (if (eq? x (- nx 1)) c (+ c 1)))
     (set! n (if (eq? y 0) c (- c nx)))
     (set! s (if (eq? y (- ny 1)) c (+ c nx)))
     (set! b (if (eq? z 0) c (- c (* nx ny))))
     (set! t (if (eq? z (- nz 1)) c (+ c (* nx ny))))
     (array-set-host!
      out
      c
      (+
       (* cc (array-ref-host in c))
       (* cw (array-ref-host in w))
       (* ce (array-ref-host in e))
       (* cs (array-ref-host in s))
       (* cn (array-ref-host in n))
       (* cb (array-ref-host in b))
       (* ct (array-ref-host in t))))))))

(define (diffusion3d-baseline count in out nx ny nz ce cw cn cs ct cb cc)
  (for
   ((i (in-range count)))
   (diffusion3d-baseline-iteration in out nx ny nz ce cw cn cs ct cb cc)
   (define temp in)
   (set! in out)
   (set! out temp))
  (when (eq? (modulo count 2) 0) (set! out in)))

(define (array-eq-verify arr1 arr2 len)
  (for
   ((i (in-range len)))
   (assert (eq? (array-ref-host arr1 i) (array-ref-host arr2 i)))))

(define (r) (define-symbolic* r real?) r)

(define-values (SIZEX SIZEY SIZEZ) (values 6 6 5))

(define SIZE (* SIZEX SIZEY SIZEZ))

(define-values (BLOCKSIZEX BLOCKSIZEY) (values 3 3))

(define CPU-in (make-array (for/vector ((i SIZE)) (make-element (r))) SIZE))

(define GPU-in
  (make-array
   (for/vector ((i SIZE)) (make-element (array-ref-host CPU-in i)))
   SIZE))

(define CPU-out (make-array (for/vector ((i SIZE)) (make-element i)) SIZE))

(define GPU-out (make-array (for/vector ((i SIZE)) (make-element i)) SIZE))

(define-symbolic e w n s t b c real?)

(define lst (for/list ((i SIZE)) (array-ref-host CPU-in i)))

(define (synth-stencil)
  (time
   (synthesize
    #:forall
    (append lst (list e w n s t b c))
    #:guarantee
    (begin
      (diffusion3d-baseline 1 CPU-in CPU-out SIZEX SIZEY SIZEZ e w n s t b c)
      (diffusion-run-kernel
       (list (quotient SIZEX BLOCKSIZEX) (quotient SIZEY BLOCKSIZEY))
       (list BLOCKSIZEX BLOCKSIZEY)
       1
       GPU-in
       GPU-out
       SIZEX
       SIZEY
       SIZEZ
       e
       w
       n
       s
       t
       b
       c)
      (array-eq-verify CPU-out GPU-out SIZE)))))

(define res (list-ref (map syntax->datum (generate-forms (synth-stencil))) 0))

(write-synth-result res "diffusion3d-shared-test.rkt")

