#lang rosette

(require "../../lang.rkt" "diffusion3d-baseline.rkt")

;; 7-points stencil computation example.

;; Parallel diffusion program
(define (diffusion-kernel in
                          out
                          nx ny nz
                          ce cw cn cs ct cb cc
                          file
                          file2
                          file3
                          file4
                          file5)
  (:= int tid-x (thread-idx 0))
  (:= int tid-y (thread-idx 1))
  (:= int BLOCKSIZE (*/LS (block-dim 0) (block-dim 1)))
  (:= int c2 (+/LS (*/LS tid-y (block-dim 0)) tid-x))
  (:= int i (+/LS (*/LS (block-dim 0) (block-idx 0)) (thread-idx 0)))
  (:= int j (+/LS (*/LS (block-dim 1) (block-idx 1)) (thread-idx 1)))
  (:= int c (+/LS (*/LS j nx) i))
  (:= int xy (*/LS nx ny))
  
  (:shared float [smem BLOCKSIZE])
  
  (for ([k (in-range nz)])
    (= [smem c2] [in c])
    (syncthreads)
    (:= int w (?: (eq?/LS i 0) c (-/LS c 1)))
    (:= int e (?: (eq?/LS i (-/LS nx 1)) c (+/LS c 1)))
    (:= int n (?: (eq?/LS j 0) c (-/LS c nx)))
    (:= int s (?: (eq?/LS j (-/LS ny 1)) c (+/LS c nx)))
    (:= int b (?: (eq?/LS k 0) c (-/LS c xy)))
    (:= int t (?: (eq?/LS k (-/LS nz 1)) c (+/LS c xy)))
    (= [out c] (+/LS (*/LS cc [in c])
                     ; tid bid id smid i j c threadIdx.x threadIdx.y c2 blockDim.x blockDim.y nx ny nz
                     (*/LS cw (profiling-access "0" in w i j c tid-x tid-y c2 (block-dim 0) nx ny))
                     (*/LS ce (profiling-access "1" in e i j c tid-x tid-y c2 (block-dim 0) nx ny))
                     (*/LS cn (profiling-access "3" in n i j c tid-x tid-y c2 (block-dim 0) nx ny))
                     (*/LS cs (profiling-access "4" in s i j c tid-x tid-y c2 (block-dim 0) nx ny))
                     (*/LS cb (profiling-access "5" in b i j c tid-x tid-y c2 (block-dim 0) nx ny))
                     (*/LS ct [in t])))
    (+=/LS c xy)))

(define (diffusion-run-kernel file
                              file2
                              file3
                              file4
                              file5
                              grid
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
                   ce cw cn cs ct cb cc
                   file
                   file2
                   file3
                   file4
                   file5)
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

(define k 3) ; upper bound
(define-symbolic* v integer? [k])
(define-symbolic n0 integer?)
(define vn (take v n0)) ; list of up to k integers
(define (len)
  (length vn))

(define-values (GRIDSIZEX GRIDSIZEY) (values 4 3))
(define-values (BLOCKSIZEX BLOCKSIZEY) (values 4 3))

(define-values (SIZEX SIZEY SIZEZ) (values (* BLOCKSIZEX GRIDSIZEX) (* BLOCKSIZEY GRIDSIZEY) 3))
(define SIZE (* SIZEX SIZEY SIZEZ))

(define CPU-in (make-array (for/vector ([i SIZE]) (make-element (r))) SIZE))
(define GPU-in (make-array (for/vector ([i SIZE]) (make-element (array-ref-host CPU-in i))) SIZE))
(define CPU-out (make-array (for/vector ([i SIZE]) (make-element 0)) SIZE))
(define GPU-out (make-array (for/vector ([i SIZE]) (make-element 0)) SIZE))

(define-symbolic e w n s t b c real?)
;(define-values (e w n s t b c) (values 1 1 1 1 1 1 1))

;; Execute a diffusion program on CPU
;(diffusion3d-baseline 2
;                      CPU-in CPU-out
;                      SIZEX SIZEY SIZEZ
;                      e w n s t b c)

;; Execute a diffusion program on GPU
(make-directory* "profiles")

(define (new-counter)
  (let ([n 0])
    (lambda ()
      (set! n (+ n 1))
      n)))

(define counter (new-counter))

(define out-file (open-output-file (string-append "profiles/profile" (number->string (counter))) #:exists 'truncate))
(define out-file2 (open-output-file (string-append "profiles/profile" (number->string (counter))) #:exists 'truncate))
(define out-file3 (open-output-file (string-append "profiles/profile" (number->string (counter))) #:exists 'truncate))
(define out-file4 (open-output-file (string-append "profiles/profile" (number->string (counter))) #:exists 'truncate))
(define out-file5 (open-output-file (string-append "profiles/profile" (number->string (counter))) #:exists 'truncate))
;(define out-file (open-output-file "profile" #:exists 'truncate))
(fprintf out-file "tid bid id smid i j c tid-x tid-y c2 blockDim.x nx ny\n")
(fprintf out-file2 "tid bid id smid i j c tid-x tid-y c2 blockDim.x nx ny\n")
(fprintf out-file3 "tid bid id smid i j c tid-x tid-y c2 blockDim.x nx ny\n")
(fprintf out-file4 "tid bid id smid i j c tid-x tid-y c2 blockDim.x nx ny\n")
(fprintf out-file5 "tid bid id smid i j c tid-x tid-y c2 blockDim.x nx ny\n")
(time (diffusion-run-kernel out-file
                            out-file2
                            out-file3
                            out-file4
                            out-file5
                            (list GRIDSIZEX GRIDSIZEY)
                            (list BLOCKSIZEX BLOCKSIZEY)
                            1
                            GPU-in GPU-out
                            SIZEX SIZEY SIZEZ
                            e w n s t b c))
(close-output-port out-file)
(close-output-port out-file2)
(close-output-port out-file3)
(close-output-port out-file4)
(close-output-port out-file5)




;(define (diffusion-verify) (time (verify (array-eq-verify CPU-in GPU-in SIZE))))

;(print-matrix CPU-in 8 8)
;(newline)
;(print-matrix GPU-in 8 8)
