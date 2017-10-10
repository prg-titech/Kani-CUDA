#lang rosette

(require "diffusion3d-baseline.rkt"
         "../../lang.rkt")

(current-bitwidth 8)

(define (diffusion-kernel-temporal-blocking in
                                            out
                                            nx ny nz
                                            ce cw cn cs ct cb cc)
  ;; Number of elements in xy plane of input
  (:= int xy (*/LS nx ny))
  
  (: int i j c sc)
  (: int i2 j2 c2 sc2)
  
  (:= int NUM_SMEM 3) 
  (:shared real sb[NUM_SMEM][(*/LS (block-dim 0) (block-dim 1))])
  (:= int sb1 0)
  (:= int sb2 1)
  (:= int sb3 2)
  
  ;(= i (+/LS (*/LS (-/LS (block-dim 0) (choose 0 2)) (block-idx 0)) (thread-idx 0) (choose 0 -1)))
  (= i (+/LS (*/LS (-/LS (block-dim 0) 2) (block-idx 0)) (thread-idx (choose 0 1)) -1))
  (= i (max/LS i 0))
  (= i (min/LS i (-/LS nx 1)))
  (= j (+/LS (*/LS (-/LS (block-dim 1) 2) (block-idx 1)) (thread-idx 1) -1))
  (= j (max/LS j 0))
  (= j (min/LS j (-/LS ny 1)))
  
  ;; Index of 
  (= c (+/LS i (*/LS j nx)))
  (= sc (+/LS (thread-idx 0) (*/LS (thread-idx 1) (block-dim 0))))
  ;(printf "i = ~a\n" i)
  ;(printf "j = ~a\n" j)
  ;(printf "c = ~a\n" c)
  ;(printf "sc = ~a\n" sc)
  
  (= i2 (+/LS (*/LS (-/LS (block-dim 0) 2) (block-idx 0)) (min/LS (thread-idx 0) (-/LS (block-dim 0) 3))))
  (= i2 (min/LS i2 (-/LS nx 1)))
  (= j2 (+/LS (*/LS (-/LS (block-dim 1) 2) (block-idx 1)) (min/LS (thread-idx 1) (-/LS (block-dim 1) 3))))
  (= j2 (min/LS j2 (-/LS ny 1)))
  ;(printf "i2 = ~a\n" i2)
  ;(printf "j2 = ~a\n" j2)
  (= c2 (+/LS i2 (*/LS j2 nx)))
  (= sc2 (+/LS (modulo/LS i2 (-/LS (block-dim 0) 2))
               (*/LS (block-dim 0) (+/LS (modulo/LS j2 (-/LS (block-dim 1) 2)) 1))
               1))
  ;(printf "sc = ~a\n" sc)
  ;(printf "c2 = ~a\n" c2)
  
  (:= int w (?: (eq?/LS i (choose 0 (-/LS (choose nx ny nz) 1))) c ((choose +/LS -/LS) c (choose nx xy 1))))
  (:= int e (?: (eq?/LS i (choose 0 (-/LS (choose nx ny nz) 1))) c ((choose +/LS -/LS) c (choose nx xy 1))))
  (:= int n (?: (eq?/LS j (choose 0 (-/LS (choose nx ny nz) 1))) c ((choose +/LS -/LS) c (choose nx xy 1))))
  (:= int s (?: (eq?/LS j (choose 0 (-/LS (choose nx ny nz) 1))) c ((choose +/LS -/LS) c (choose nx xy 1))))
  (:= int b c)
  (:= int t (?: (eq?/LS nz 1) c (+/LS c xy)))
  
  (:= real v (+/LS (*/LS cc [in c]) (*/LS cw [in w]) (*/LS ce [in e]) (*/LS cs [in s])
                   (*/LS cn [in n]) (*/LS cb [in b]) (*/LS ct [in t])))
  (= [sb sb2 sc] v)
  (+= c xy)
  
  (:= int k 1)
  (for- [: (</LS k nz) : (++ k)]
        (:= int w (?: (eq?/LS i (choose 0 (-/LS (choose nx ny nz) 1))) c ((choose +/LS -/LS) c (choose nx xy 1))))
        (:= int e (?: (eq?/LS i (choose 0 (-/LS (choose nx ny nz) 1))) c ((choose +/LS -/LS) c (choose nx xy 1))))
        (:= int n (?: (eq?/LS j (choose 0 (-/LS (choose nx ny nz) 1))) c ((choose +/LS -/LS) c (choose nx xy 1))))
        (:= int s (?: (eq?/LS j (choose 0 (-/LS (choose nx ny nz) 1))) c ((choose +/LS -/LS) c (choose nx xy 1))))
        (:= int b (?: (eq?/LS k (choose 0 (-/LS (choose nx ny nz) 1))) c ((choose +/LS -/LS) c (choose nx xy 1))))
        (:= int t (?: (eq?/LS k (choose 0 (-/LS (choose nx ny nz) 1))) c ((choose +/LS -/LS) c (choose nx xy 1))))
        (:= real v (+/LS (*/LS cc [in c]) (*/LS cw [in w]) (*/LS ce [in e]) (*/LS cs [in s])
                         (*/LS cn [in n]) (*/LS cb [in b]) (*/LS ct [in t])))
        (= [sb sb3 sc] v)
        (+= c xy)
        
        (choose (barrier) (void))
        
        (= w (?: (eq?/LS i2 (choose 0 (-/LS (choose nx ny nz) 1))) sc2 (-/LS sc2 1)))
        (= e (?: (eq?/LS i2 (choose 0 (-/LS (choose nx ny nz) 1))) sc2 (+/LS sc2 1)))
        (= n (?: (eq?/LS j2 (choose 0 (-/LS (choose nx ny nz) 1))) sc2 (-/LS sc2 (block-dim 0))))
        (= s (?: (eq?/LS j2 (choose 0 (-/LS (choose nx ny nz) 1))) sc2 (+/LS sc2 (block-dim 0))))
        
        (:= int bv (?: (eq?/LS (-/LS k 1) 0) sb2 sb1))
        (:= int tv sb3)
        ;(printf "c2 = ~a\n" c2)
        ;(printf "sc2 = ~a\n" sc2)
        ;(printf "w = ~a\n" w)
        ;(printf "e = ~a\n" e)
        ;(printf "n = ~a\n" n)
        ;(printf "s = ~a\n" s)
        (if- (&&/LS (</LS (thread-idx 0) (-/LS (block-dim 0) 2)) (</LS (thread-idx 1) (-/LS (block-dim 1) 2)))
             (= [out c2] (+/LS (*/LS cc [sb sb2 sc2]) (*/LS cw [sb sb2 w]) (*/LS ce [sb sb2 e]) (*/LS cs [sb sb2 s])
                               (*/LS cn [sb sb2 n]) (*/LS cb [sb bv sc2]) (*/LS ct [sb tv sc2]))))
        ;(printf "Pass!\n")
        (+= c2 xy)
        (barrier)
        
        (:= int sb-temp sb1)
        (= sb1 sb2)
        (= sb2 sb3)
        (= sb3 sb-temp))
  
  (= w (?: (eq?/LS i2 (choose 0 (-/LS (choose nx ny nz) 1))) sc2 (-/LS sc2 1)))
  (= e (?: (eq?/LS i2 (choose 0 (-/LS (choose nx ny nz) 1))) sc2 (+/LS sc2 1)))
  (= n (?: (eq?/LS j2 (choose 0 (-/LS (choose nx ny nz) 1))) sc2 (-/LS sc2 (block-dim 0))))
  (= s (?: (eq?/LS j2 (choose 0 (-/LS (choose nx ny nz) 1))) sc2 (+/LS sc2 (block-dim 0))))
  (:= int bv sb1)
  (:= int tv sb2)
  ;(printf "sb2 = ~a\n" sb2)
  (if- (&&/LS (</LS (thread-idx 0) (-/LS (block-dim 0) 2)) (</LS (thread-idx 1) (-/LS (block-dim 1) 2)))
       (= [out c2] (+/LS (*/LS cc [sb sb2 sc2]) (*/LS cw [sb sb2 w]) (*/LS ce [sb sb2 e]) (*/LS cs [sb sb2 s])
                         (*/LS cn [sb sb2 n]) (*/LS cb [sb bv sc2]) (*/LS ct [sb tv sc2])))))




(define (diffusion-run-kernel grid
                              block
                              count
                              in out
                              nx ny nz
                              ce cw cn cs ct cb cc)
  (for ([i (in-range count)])
    (invoke-kernel diffusion-kernel-temporal-blocking
                   grid
                   block
                   in out
                   nx ny nz
                   ce cw cn cs ct cb cc)
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

(define-values (SIZEX SIZEY SIZEZ) (values 3 3 2))
(define SIZE (* SIZEX SIZEY SIZEZ))

(define-values (BLOCKSIZEX BLOCKSIZEY) (values 4 4))

(define CPU-in (make-array (for/vector ([i SIZE]) (make-element (r))) SIZE))
(define GPU-in (make-array (for/vector ([i SIZE]) (make-element (array-ref-host CPU-in i))) SIZE))
(define CPU-out (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
(define GPU-out (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))

(define-symbolic e w n s t b c real?)
;(define-values (e w n s t b c) (values 1 1 1 1 1 1 1))

;; Execute a diffusion program on CPU


;; Execute a diffusion program on GPU
(define lst
  (for/list ([i SIZE])
    (array-ref-host CPU-in i)))
    

(define (synth-stencil)
  (time 
   (synthesize #:forall (append lst (list e w n s t b c))
               #:guarantee (begin
                             (diffusion3d-baseline 2
                                                   CPU-in CPU-out
                                                   SIZEX SIZEY SIZEZ
                                                   e w n s t b c)
                             (diffusion-run-kernel '(1 1)
                                                   '(5 5)
                                                   1
                                                   GPU-in GPU-out
                                                   SIZEX SIZEY SIZEZ
                                                   e w n s t b c)
                             (array-eq-verify
                              CPU-in GPU-out SIZE)))))


(map syntax->datum (generate-forms (synth-stencil)))