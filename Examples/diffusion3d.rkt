#lang rosette

(require "../lang.rkt")

;; 7-points stencil computation example.

;; Sequential diffusion program.
(define (diffusion3d-baseline-iteration in
                                        out
                                        nx ny nz
                                        ce cw cn cs ct cb cc)
  (for ([z (in-range nz)])
    (for ([y (in-range ny)])
      (for ([x (in-range nx)])
        (define-values (c w e n s b t) (values 0 0 0 0 0 0 0))
        (set! c (+ x (* y nx) (* z nx ny)))
        (set! w (if (eq? x 0) c (- c 1)))
        (set! e (if (eq? x (- nx 1)) c (+ c 1)))
        (set! n (if (eq? y 0) c (- c nx)))
        (set! s (if (eq? y (- ny 1)) c (+ c nx)))
        (set! b (if (eq? z 0) c (- c (* nx ny))))
        (set! t (if (eq? z (- nz 1)) c (+ c (* nx ny))))
        (array-set-host! out c
                         (+ (* cc (array-ref-host in c))
                            (* cw (array-ref-host in w))
                            (* ce (array-ref-host in e))
                            (* cs (array-ref-host in s))
                            (* cn (array-ref-host in n))
                            (* cb (array-ref-host in b))
                            (* ct (array-ref-host in t))))))))

(define (diffusion3d-baseline count
                              in
                              out
                              nx ny nz
                              ce cw cn cs ct cb cc)
  (for ([i (in-range count)])
    (diffusion3d-baseline-iteration in
                                    out
                                    nx ny nz
                                    ce cw cn cs ct cb cc)
    (set! in out)))


;; Parallel diffusion program
(define (diffusion-kernel in
                          out
                          nx ny nz
                          ce cw cn cs ct cb cc)
  (:= int i (+/LS (*/LS (block-dim 0) (block-idx 0)) (thread-idx 0)))
  (:= int j (+/LS (*/LS (block-dim 1) (block-idx 1)) (thread-idx 1)))
  (:= int c (+/LS (*/LS j nx) i))
  (:= int xy (*/LS nx ny))
  (for ([k (in-range nz)])
    (:= int w (?: (eq?/LS i 0) c (-/LS c 1)))
    (:= int e (?: (eq?/LS i (-/LS nx 1)) c (+/LS c 1)))
    (:= int n (?: (eq?/LS j 0) c (-/LS c nx)))
    (:= int s (?: (eq?/LS j (-/LS ny 1)) c (+/LS c nx)))
    (:= int b (?: (eq?/LS k 0) c (-/LS c xy)))
    (:= int t (?: (eq?/LS k (-/LS nz 1)) c (+/LS c xy)))
    (= [out c] (+/LS (*/LS cc [in c]) (*/LS cw [in w]) (*/LS ce [in e]) (*/LS cs [in s])
                     (*/LS cn [in n]) (*/LS cb [in b]) (*/LS ct [in t])))
    (+= c xy)))

(define (diffusion-run-kernel count
                              in
                              out
                              nx ny nz
                              ce cw cn cs ct cb cc)
  (for ([i (in-range count)])
    (diffusion-kernel in
                      out
                      nx ny nz
                      ce cw cn cs ct cb cc)
    (set! in out)))

;; Add a constraint that it is equal to each of the elements of
;; two arrays, arr1 and arr2, to asserts.
(define (array-eq-verify arr1 arr2 len)
  (define cont1 (array-contents arr1))
  (define cont2 (array-contents arr2))
  (for ([i (in-range len)])
    (assert
     (eq?
      (element-content (vector-ref cont1 i))
      (element-content (vector-ref cont2 i))))))

(define (r)
  (define-symbolic* r real?)
  r)

(define CPU-in (make-array (for/vector ([i 64]) (make-element (r))) 64))
(define GPU-in (make-array (for/vector ([i 64]) (make-element (array-ref-host CPU-in i))) 64))
(define CPU-out (make-array (for/vector ([i 64]) (make-element 0)) 64))
(define GPU-out (make-array (for/vector ([i 64]) (make-element 0)) 64))

(define-symbolic e w n s t b c real?)

;; Execute a diffusion program on CPU
(diffusion3d-baseline 1
                      CPU-in CPU-out
                      4 4 4
                      e w n s t b c)

;; Execute a diffusion program on GPU
(invoke-kernel diffusion-run-kernel
               '(2 2)
               '(2 2)
               1
               GPU-in GPU-out
               4 4 4
               e w n s t b c)

(verify (array-eq-verify CPU-out GPU-out 64))

;(printmatrix CPU-out 1 64)
;(printmatrix GPU-out 1 64)
