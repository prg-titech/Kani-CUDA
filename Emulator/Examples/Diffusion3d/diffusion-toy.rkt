#lang rosette

(require "diffusion3d-baseline.rkt"
         "../../lang.rkt"
         (rename-in rosette/lib/synthax [choose ?]))

(current-bitwidth #f)

;; TODO minimize syntax for stencil 
;; Template example of teranry operator
;; (hoge == hoge)? hoge : hoge
(define-synthax (stencil-a-exp x y z w c depth)
  ;  (?:
  ;   (eq?/LS x 0)
  ;   c
  ;   ((? +/LS -/LS) c 1)))
  #:base
  (?:
   ;; condition
   (eq?/LS x (? 0 ((? +/LS -/LS) (? y z w) 1)))
   ;; then-exp
   c
   ;; else-exp
   ((? +/LS -/LS) c (? y z w 1)))
  #:else
  (?:
   (eq?/LS x (? 0 ((? +/LS -/LS) (? y z w) 1)))
   c
   ((? +/LS -/LS) c (? y z w 1))))



;; Template example of bool-expression
;; (hoge == hoge) && (! (hoge == hoge))
(define-synthax (stencil-b-exp x y z w v depth)
  #:base
  (&&/LS
   (? (eq?/LS
       x
       (+/LS (? y z w 0) (? -1 0 1)))
      (eq?/LS
       y
       (+/LS (? z w 0) (? -1 0 1)))
      (eq?/LS
       z
       (+/LS (? w 0) (? -1 0 1))))
   (!/LS
    (? (eq?/LS
        x
        (+/LS (? y z w 0) (? -1 0 1)))
       (eq?/LS
        y
        (+/LS (? z w 0) (? -1 0 1)))
       (eq?/LS
        z
        (+/LS (? w 0) (? -1 0 1))))))
  #:else
  (&&/LS
   (? (eq?/LS
       x
       (+/LS (? y z w 0) (? -1 0 1)))
      (eq?/LS
       y
       (+/LS (? z w 0) (? -1 0 1)))
      (eq?/LS
       z
       (+/LS (? w 0) (? -1 0 1))))
   (!/LS
    (? (eq?/LS
        x
        (+/LS (? y z w 0) (? -1 0 1)))
       (eq?/LS
        y
        (+/LS (? z w 0) (? -1 0 1)))
       (eq?/LS
        z
        (+/LS (? w 0) (? -1 0 1)))))))

(define-synthax (nnf a b c depth)
  #:base (? a b c)
  #:else (?
          a b c
          (!/LS (nnf a b c (- depth 1)))
          ((? &&/LS eq?/LS) (nnf a b c (- depth 1))
                            (nnf a b c (- depth 1)))))

(define-synthax (holes x y z w v depth)
  #:base (+/LS (? x y z w v 0) (? -1 0 1))
  #:else (+/LS (? x y z w v 0) (? -1 0 1)))

(define (hole x y z w v)
  (holes x y z w v 0))


(define (nnf=> a b c)
  (nnf a b c 1))


(define (diffusion-kernel in out nx ny nz ce cw cn cs ct cb cc)
  (:= int BLOCKSIZE (*/LS (block-dim 0) (block-dim 1)))
  (:shared real smem (BLOCKSIZE))
  
  (:= int i (+/LS (*/LS (block-dim 0) (block-idx 0)) (thread-idx 0)))
  (:= int j (+/LS (*/LS (block-dim 1) (block-idx 1)) (thread-idx 1)))
  (:= int c (+/LS (*/LS j nx) i))
  ;; int c2 = threadIdx.y * blockDim.x + threadIdx.x;
  (:= int c2 (+/LS (*/LS (thread-idx 1) (block-dim 0)) (thread-idx 0)))
  (:= int xy (*/LS nx ny))
  
  (: int tb tc tt)
  (= tt (in (+/LS c xy)))
  (= tc (in c))
  (= tb tc)
  
  (? (barrier) (void))
  
  (= (smem c2) tc)
  
  (? (barrier) (void))
  
    (:= bool bw (stencil-b-exp i (thread-idx 0) (block-dim 0) nx c2 0))
    (:= bool be (stencil-b-exp i (thread-idx 0) (block-dim 0) nx c2 0))
    (:= bool bn (stencil-b-exp j (thread-idx 1) (block-dim 1) ny c2 0))
    (:= bool bs (stencil-b-exp j (thread-idx 1) (block-dim 1) ny c2 0))
  
;  (:= bool bw (&&/LS (eq?/LS (thread-idx 0) 0) (!/LS (eq?/LS i 0))))
 ; (:= bool be (&&/LS (eq?/LS (thread-idx 0) (-/LS (block-dim 0) 1)) (!/LS (eq?/LS i (-/LS nx 1)))))
  ;(:= bool bn (&&/LS (eq?/LS (thread-idx 1) 0) (!/LS (eq?/LS j 0))))
  ;(:= bool bs (&&/LS (eq?/LS (thread-idx 1) (-/LS (block-dim 1) 1)) (!/LS (eq?/LS j (-/LS ny 1)))))
  
  ;(:= int w (?: (eq?/LS i 0) c2 (-/LS c2 1)))
  (:= int w (?:
             ;; condition
             (eq?/LS i (? 0 ((? +/LS -/LS) (? (thread-idx 0) (block-dim 0) nx) 1)))
             ;; then-exp
             c2
             ;; else-exp
             (-/LS c2 1)))
  ;  (print w)
  (:= int e (?: (eq?/LS i (-/LS nx 1)) c2 (+/LS c2 1)))
  (:= int n (?: (eq?/LS j 0) c2 (-/LS c2 (block-dim 0))))
  (:= int s (?: (eq?/LS j (-/LS nx 1)) c2 (+/LS c2 (block-dim 0))))
  
  (barrier)
  
  (=
   (out c)
   (+/LS
    (*/LS cc tc)
    (*/LS cw (?: bw (in (-/LS c 1)) (smem w)))
    (*/LS ce (?: be (in (+/LS c 1)) (smem e)))
    (*/LS cn (?: bn (in (-/LS c nx)) (smem n)))
    (*/LS cs (?: bs (in (+/LS c nx)) (smem s)))
    (*/LS cb tb)
    (*/LS ct tt)))
  (+= c xy)
  (:= int k 1)
  
  (for-
   (: (</LS k (-/LS nz 1)) : (++ k))
   (= tb tc)
   (= tc tt)
   
   (barrier)
   
   (= (smem c2) tt)
   (= tt (in (+/LS c xy)))
   
   (barrier)
   
   (=
    (out c)
    (+/LS
     (*/LS cc tc)
     (*/LS cw (?: bw (in (-/LS c 1)) (smem w)))
     (*/LS ce (?: be (in (+/LS c 1)) (smem e)))
     (*/LS cn (?: bn (in (-/LS c nx)) (smem n)))
     (*/LS cs (?: bs (in (+/LS c nx)) (smem s)))
     (*/LS cb tb)
     (*/LS ct tt)))
   
   (barrier)
   
   (+= c xy))
  
  (= tb tc)
  (= tc tt)
  (void)
  (= (smem c2) tt)
  (barrier)
  (=
   (out c)
   (+/LS
    (*/LS cc tc)
    (*/LS cw (?: bw (in (-/LS c 1)) (smem w)))
    (*/LS ce (?: be (in (+/LS c 1)) (smem e)))
    (*/LS cn (?: bn (in (-/LS c nx)) (smem n)))
    (*/LS cs (?: bs (in (+/LS c nx)) (smem s)))
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

(define-values (SIZEX SIZEY SIZEZ) (values 8 8 3))
(define SIZE (* SIZEX SIZEY SIZEZ))

(define-values (BLOCKSIZEX BLOCKSIZEY) (values 4 4))

(define CPU-in (make-array (for/vector ([i SIZE]) (make-element (r))) SIZE))
(define GPU-in (make-array (for/vector ([i SIZE]) (make-element (array-ref-host CPU-in i))) SIZE))
(define CPU-out (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
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






;; 0  1  2     0  1  3
;; 3  4  5 =>  6  7  9  
;; 6  7  8    12 13 15
;; Input array is a flat array.
(define (test arr)
  ; Get thread ID
  (:= int i (thread-idx 0))
  
  ; Desired code
  ; int a = (i % 3 == 0) ? i : (i - 1);
  
  ; Expression with hole
  ; (? (i % 3) 0 1 2 i)
  (:= int a (stencil-a-exp (modulo/LS i 3) (make-vector 9 1) (make-vector 9 0) 2 i 0))
  ; (:= int a (?: (eq?/LS (modulo/LS i 3) 0) i (-/LS i 1)))
  
  (:= int b [arr a])
  (? (barrier) (void))
  (= [arr i] (+/LS [arr i] b)))


(define (synth-test)
  (time
   (synthesize #:forall '()
               #:guarantee (begin
                             (define SIZE 9)
                             (define in (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
                             ;                             (define out (make-array (for/vector ([i SIZE])
                             ;                                                       (if (eq? (modulo i 2) 0)
                             ;                                                           (make-element 0)
                             ;                                                           (make-element 1))) SIZE))
                             (define out (make-array (vector (make-element 0)
                                                             (make-element 1)
                                                             (make-element 3)
                                                             (make-element 6)
                                                             (make-element 7)
                                                             (make-element 9)
                                                             (make-element 12)
                                                             (make-element 13)
                                                             (make-element 15)) SIZE))
                             (invoke-kernel test '(1) '(9) in)
                             (array-eq-verify in out SIZE)
                             ))))


;(map syntax->datum (generate-forms (synth-stencil)))

;(generate-forms (synth-test))

;(define-symbolic b2 b3 boolean?)
;
;(if b2 (+ 1 1) (- 2 1))
;
;(cond ([b2 (+ 1 1)]
;       [(! b2) (+ 1 2)]
;       [else 0]))
;
;(cond [(and (! (and b2 b3)) (! (and (! b2) b3))) (+ 1 1)]
;        [(! (and b2 b3)) (+ 2 1)]
;        [(! (and (! b2) b3)) (+ 3 1)]
;        [else 0])


