#lang rosette

(require "../Emulator/lang.rkt")
(delete-directory/files "profiles" #:must-exist? #f)
(make-directory* "profiles")
(func
 void
 __global__diffusion_kernel
 ((float in)
  (float out)
  (int nx)
  (int ny)
  (int nz)
  (float ce)
  (float cw)
  (float cn)
  (float cs)
  (float ct)
  (float cb)
  (float cc))
 (profile-vars "threadIdx.x threadIdx.y blockDim.x blockDim.y csb c i j")
 (begin (:= int i (+/LS (*/LS (block-dim 0) (block-idx 0)) (thread-idx 0))))
 (begin (:= int j (+/LS (*/LS (block-dim 1) (block-idx 1)) (thread-idx 1))))
 (begin (:= int c (+/LS i (*/LS j nx))))
 (begin (:= int xy (*/LS nx ny)))
 (begin (:shared float (sb (*/LS 3 4))))
 (begin (:= int csb (+/LS (thread-idx 0) (*/LS (thread-idx 1) (block-dim 0)))))
 (for-
  ((begin (:= int k 0)) : (</LS k nz) : (++/LS k))
  (= (sb csb) (in c))
  (begin (:= int w (?: (==/LS i 0) c (-/LS c 1))))
  (begin (:= int e (?: (==/LS i (-/LS nx 1)) c (+/LS c 1))))
  (begin (:= int n (?: (==/LS j 0) c (-/LS c nx))))
  (begin (:= int s (?: (==/LS j (-/LS ny 1)) c (+/LS c nx))))
  (begin (:= int b (?: (==/LS k 0) c (-/LS c xy))))
  (begin (:= int t (?: (==/LS k (-/LS nz 1)) c (+/LS c xy))))
  (=
   (out c)
   (+/LS
    (+/LS
     (+/LS
      (+/LS
       (+/LS
        (+/LS
         (*/LS cc (in c))
         (*/LS
          cw
          (profiling-access
           "__opt__817409"
           in
           w
           (thread-idx 0)
           (thread-idx 1)
           (block-dim 0)
           (block-dim 1)
           csb
           c
           i
           j)))
        (*/LS
         ce
         (profiling-access
          "__opt__435794"
          in
          e
          (thread-idx 0)
          (thread-idx 1)
          (block-dim 0)
          (block-dim 1)
          csb
          c
          i
          j)))
       (*/LS
        cs
        (profiling-access
         "__opt__496823"
         in
         s
         (thread-idx 0)
         (thread-idx 1)
         (block-dim 0)
         (block-dim 1)
         csb
         c
         i
         j)))
      (*/LS
       cn
       (profiling-access
        "__opt__343313"
        in
        n
        (thread-idx 0)
        (thread-idx 1)
        (block-dim 0)
        (block-dim 1)
        csb
        c
        i
        j)))
     (*/LS cb (in b)))
    (*/LS ct (in t))))
  (+=/LS c xy)))
(func
 void
 initialize
 ((float buff)
  (int nx)
  (int ny)
  (int nz)
  (float kx)
  (float ky)
  (float kz)
  (float dx)
  (float dy)
  (float dz)
  (float kappa)
  (float time))
 (begin (define ax (exp (* (* (- kappa) time) (* kx kx)))))
 (begin (define ay (exp (* (* (- kappa) time) (* ky ky)))))
 (begin (define az (exp (* (* (- kappa) time) (* kz kz)))))
 (begin (define jz 0))
 (for-
  ((set! jz 0) : (< jz nz) : (++ jz))
  (begin (define jy 0))
  (for-
   ((set! jy 0) : (< jy ny) : (++ jy))
   (begin (define jx 0))
   (for-
    ((set! jx 0) : (< jx nx) : (++ jx))
    (begin (define j (+ (+ (* (* jz nx) ny) (* jy nx)) jx)))
    (begin (define x (* dx (+ jx 0.5))))
    (begin (define y (* dy (+ jy 0.5))))
    (begin (define z (* dz (+ jz 0.5))))
    (begin
      (define f0
        (*
         (*
          (* 0.125 (- 1.0 (* ax (cos (* kx x)))))
          (- 1.0 (* ay (cos (* ky y)))))
         (- 1.0 (* az (cos (* kz z)))))))
    (array-set-host! buff j (__symbol))))))
(func
 int
 main
 ()
 (begin (define count 3))
 (begin (define nx 0) (define ny 0) (define nz 0))
 (set! nx (* 3 3))
 (set! ny (* 4 3))
 (set! nz 4)
 (begin (define l 0) (define kappa 0))
 (begin (define kx 0) (define ky 0) (define kz 0))
 (begin (define dx 0) (define dy 0) (define dz 0) (define dt 0))
 (begin
   (define ce 0)
   (define cw 0)
   (define cn 0)
   (define cs 0)
   (define ct 0)
   (define cb 0)
   (define cc 0))
 (set! l 1.0)
 (set! kappa 0.1)
 (set! dx (/ l nx))
 (set! dy (/ l ny))
 (set! dz (/ l nz))
 (set! kx (* 2.0 3.141592653589793))
 (set! ky (* 2.0 3.141592653589793))
 (set! kz (* 2.0 3.141592653589793))
 (set! dt (/ (* (* 0.1 dx) dy) kappa))
 (set! ce (/ (* kappa dt) (* dx dx)))
 (set! cw (/ (* kappa dt) (* dx dx)))
 (set! cn (/ (* kappa dt) (* dy dy)))
 (set! cs (/ (* kappa dt) (* dy dy)))
 (set! ct (/ (* kappa dt) (* dz dz)))
 (set! cb (/ (* kappa dt) (* dz dz)))
 (set! cc (- 1.0 (+ (+ (+ (+ (+ ce cw) cn) cs) ct) cb)))
 (begin (:* float in) (:* float dev_in) (:* float dev_out))
 (begin (define s (* (* (* 1 nx) ny) nz)))
 (set! in (malloc s))
 (initialize in nx ny nz kx ky kz dx dy dz kappa 0.0)
 (cudaMalloc dev_in s)
 (cudaMalloc dev_out s)
 (cudaMemcpy dev_in in s 1)
 (define block (list 3 4 1))
 (define grid (list 3 3 1))
 (for-
  ((begin (define i 0)) : (< i count) : (++ i))
  (invoke-kernel
   __global__diffusion_kernel
   grid
   block
   dev_in
   dev_out
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
  (begin (:* float t))
  (set! t dev_in)
  (set! dev_in dev_out)
  (set! dev_out t))
 (cudaMemcpy in dev_in s 0)
 0)
(main)
