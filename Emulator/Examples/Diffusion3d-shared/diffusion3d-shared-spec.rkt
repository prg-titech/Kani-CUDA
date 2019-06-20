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
    (define temp in)
    (set! in out)
    (set! out temp))
  (when (eq? (modulo count 2) 0)
    (set! out in)))

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

(define-values (SIZEX SIZEY SIZEZ) (values 6 6 5))

(define SIZE (* SIZEX SIZEY SIZEZ))

(define-values (BLOCKSIZEX BLOCKSIZEY) (values 3 3))

;; Input array on CPU
(define CPU-in (make-array (for/vector ([i SIZE]) (make-element (r))) SIZE))
;; Input array on GPU
(define GPU-in (make-array (for/vector ([i SIZE]) (make-element (array-ref-host CPU-in i))) SIZE))
;; Output array on CPU
(define CPU-out (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
;; Output array on GPU
(define GPU-out (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))

(define-symbolic e w n s t b c real?)

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


(define res
  (list-ref (map syntax->datum (generate-forms (synth-stencil))) 0))