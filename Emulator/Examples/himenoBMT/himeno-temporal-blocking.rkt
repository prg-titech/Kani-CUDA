#lang rosette

(require rackunit
         "../../lang.rkt")


(define out-file (open-output-file "profile" #:exists 'truncate))
(define BLOCKSIZE 24)
(begin
  (define imax 0)
  (define jmax 0)
  (define kmax 0)
  (define mimax 0)
  (define mjmax 0)
  (define mkmax 0))
(begin (define omega 0))

(define (jacobi
         a0
         a1
         a2
         a3
         b0
         b1
         b2
         c0
         c1
         c2
         p
         wrk1
         wrk2
         bnd
         nn
         imax
         jmax
         kmax
         omega
         gosa)
  (begin
    (: int i)
    (: int j)
    (: int k)
    (: int j2)
    (: int k2)
    (: int n)
    (: int xy)
    (: int c)
    (: int csb))
  (begin (: float s0) (: float ss) (: float temp))
  (=
   k
   (+/LS (+/LS (thread-idx 0) (*/LS (-/LS (block-dim 0) 2) (block-idx 0))) 1))
  (=
   j
   (+/LS (+/LS (thread-idx 1) (*/LS (-/LS (block-dim 1) 2) (block-idx 1))) 1))
  (= k2 (+/LS (thread-idx 0) (*/LS (block-dim 0) (block-idx 0))))
  (= j2 (+/LS (thread-idx 1) (*/LS (block-dim 1) (block-idx 1))))
  (begin (:= int tid (+/LS (-/LS k 1) (*/LS (-/LS j 1) (-/LS kmax 2)))))
  (= xy (*/LS kmax jmax))
  (:shared float (sb (* 6 4)))
  (= csb (+/LS (thread-idx 0) (*/LS (thread-idx 1) (block-dim 0))))
  (for-
   ((= n 0) : (</LS n nn) : (++/LS n))
   (= c (+/LS (*/LS j kmax) k))
   (= temp 0.0)
   (for-
    ((= i 1) : (</LS i (-/LS imax 1)) : (++/LS i))
    (syncthreads)
    (if-
     (&&/LS
      (&&/LS (&&/LS (</LS 0 (thread-idx 0)) (</LS k (-/LS kmax 1))) (</LS 0 j))
      (</LS j (-/LS jmax 1)))
     (begin
       (=
        s0
        (+/LS
         (+/LS
          (+/LS
           (+/LS
            (+/LS
             (+/LS
              (+/LS
               (+/LS
                (+/LS
                 (*/LS
                  (a0 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
                  (p
                   (+/LS
                    (+/LS (*/LS (*/LS (+/LS i 1) jmax) kmax) (*/LS j kmax))
                    k)))
                 (*/LS
                  (a1 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
                  (p
                   (+/LS
                    (+/LS (*/LS (*/LS i jmax) kmax) (*/LS (+/LS j 1) kmax))
                    k))))
                (*/LS
                 (a2 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
                 (p
                  (+/LS
                   (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax))
                   (+/LS k 1)))))
               (*/LS
                (b0 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
                (+/LS
                 (-/LS
                  (-/LS
                   (p
                    (+/LS
                     (+/LS
                      (*/LS (*/LS (+/LS i 1) jmax) kmax)
                      (*/LS (+/LS j 1) kmax))
                     k))
                   (p
                    (+/LS
                     (+/LS
                      (*/LS (*/LS (+/LS i 1) jmax) kmax)
                      (*/LS (-/LS j 1) kmax))
                     k)))
                  (p
                   (+/LS
                    (+/LS
                     (*/LS (*/LS (-/LS i 1) jmax) kmax)
                     (*/LS (+/LS j 1) kmax))
                    k)))
                 (p
                  (+/LS
                   (+/LS
                    (*/LS (*/LS (-/LS i 1) jmax) kmax)
                    (*/LS (-/LS j 1) kmax))
                   k)))))
              (*/LS
               (b1 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
               (+/LS
                (-/LS
                 (-/LS
                  (p
                   (+/LS
                    (+/LS (*/LS (*/LS i jmax) kmax) (*/LS (+/LS j 1) kmax))
                    (+/LS k 1)))
                  (p
                   (+/LS
                    (+/LS (*/LS (*/LS i jmax) kmax) (*/LS (-/LS j 1) kmax))
                    (+/LS k 1))))
                 (p
                  (+/LS
                   (+/LS (*/LS (*/LS i jmax) kmax) (*/LS (-/LS j 1) kmax))
                   (-/LS k 1))))
                (p
                 (+/LS
                  (+/LS (*/LS (*/LS i jmax) kmax) (*/LS (+/LS j 1) kmax))
                  (-/LS k 1))))))
             (*/LS
              (b2 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
              (+/LS
               (-/LS
                (-/LS
                 (p
                  (+/LS
                   (+/LS (*/LS (*/LS (+/LS i 1) jmax) kmax) (*/LS j kmax))
                   (+/LS k 1)))
                 (p
                  (+/LS
                   (+/LS (*/LS (*/LS (-/LS i 1) jmax) kmax) (*/LS j kmax))
                   (+/LS k 1))))
                (p
                 (+/LS
                  (+/LS (*/LS (*/LS (+/LS i 1) jmax) kmax) (*/LS j kmax))
                  (-/LS k 1))))
               (p
                (+/LS
                 (+/LS (*/LS (*/LS (-/LS i 1) jmax) kmax) (*/LS j kmax))
                 (-/LS k 1))))))
            (*/LS
             (c0 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
             (p
              (+/LS
               (+/LS (*/LS (*/LS (-/LS i 1) jmax) kmax) (*/LS j kmax))
               k))))
           (*/LS
            (c1 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
            (p
             (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS (-/LS j 1) kmax)) k))))
          (*/LS
           (c2 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
           (p
            (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) (-/LS k 1)))))
         (wrk1 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))))
       (=
        ss
        (*/LS
         (-/LS
          (*/LS s0 (a3 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k)))
          (p (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k)))
         (bnd (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))))
       (= temp (+/LS temp (*/LS ss ss)))
       (=
        (wrk2
         (+/LS
          (+/LS (*/LS (*/LS (*/LS (*/LS i 6) 4) 3) 6) (*/LS (*/LS j2 6) 3))
          k2))
        (+/LS
         (p (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
         (*/LS omega ss))))
     (=
        (wrk2
         (+/LS
          (+/LS (*/LS (*/LS (*/LS (*/LS i 6) 4) 3) 6) (*/LS (*/LS j2 6) 3))
          k2))
        (+/LS
         (p (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k)))))
     (+=/LS c xy))
   (syncthreads)
   
   (for-
    ((= i 1) : (</LS i (-/LS imax 1)) : (++/LS i))
    (= [sb csb]
       [wrk2 (+/LS
              (+/LS (*/LS (*/LS (*/LS (*/LS i 6) 4) 3) 6) (*/LS (*/LS j2 6) 3))
              k2)])
    (syncthreads)
    (if-
     (&&/LS
      (&&/LS
       (&&/LS
        (</LS 0 (thread-idx 0))
        (</LS (thread-idx 0) (-/LS (block-dim 0) 1)))
       (</LS 0 (thread-idx 1)))
      (</LS (thread-idx 1) (-/LS (block-dim 1) 1)))
     (begin
       (=
        s0
        (+/LS
         (+/LS
          (+/LS
           (+/LS
            (+/LS
             (+/LS
              (+/LS
               (+/LS
                (+/LS
                 (*/LS
                  (a0 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
                  (wrk2
                   (+/LS
                    (+/LS
                     (*/LS (*/LS (*/LS (*/LS (+/LS i 1) 6) 4) 3) 6)
                     (*/LS (*/LS j2 6) 3))
                    k2)))
                 (*/LS
                  (a1 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
                  (profiling-access
                   out-file
                   wrk2
                   (+/LS
                    (+/LS
                     (*/LS (*/LS (*/LS (*/LS i 6) 4) 3) 6)
                     (*/LS (*/LS j2 6) 3))
                    k2)
                   ;tid bid id smid threadIdx.x threadIdx.y blockDim.x blockDim.y csb BLOCKSIZE
                   (thread-idx 0) (thread-idx 1) (block-dim 0) (block-dim 1) csb BLOCKSIZE)))
                (*/LS
                 (a2 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
                 (wrk2
                  (+/LS
                   (+/LS
                    (*/LS (*/LS (*/LS (*/LS i 6) 4) 3) 6)
                    (*/LS (*/LS j2 6) 3))
                   (+/LS k2 1)))))
               (*/LS
                (b0 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
                (+/LS
                 (-/LS
                  (-/LS
                   (wrk2
                    (+/LS
                     (+/LS
                      (*/LS (*/LS (*/LS (*/LS (+/LS i 1) 6) 4) 3) 6)
                      (*/LS (*/LS (+/LS j2 1) 6) 3))
                     k2))
                   (wrk2
                    (+/LS
                     (+/LS
                      (*/LS (*/LS (*/LS (*/LS (+/LS i 1) 6) 4) 3) 6)
                      (*/LS (*/LS (-/LS j2 1) 6) 3))
                     k2)))
                  (wrk2
                   (+/LS
                    (+/LS
                     (*/LS (*/LS (*/LS (*/LS (-/LS i 1) 6) 4) 3) 6)
                     (*/LS (*/LS (+/LS j2 1) 6) 3))
                    k2)))
                 (wrk2
                  (+/LS
                   (+/LS
                    (*/LS (*/LS (*/LS (*/LS (-/LS i 1) 6) 4) 3) 6)
                    (*/LS (*/LS (-/LS j2 1) 6) 3))
                   k2)))))
              (*/LS
               (b1 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
               (+/LS
                (-/LS
                 (-/LS
                  (wrk2
                   (+/LS
                    (+/LS
                     (*/LS (*/LS (*/LS (*/LS i 6) 4) 3) 6)
                     (*/LS (*/LS (+/LS j2 1) 6) 3))
                    (+/LS k2 1)))
                  (wrk2
                   (+/LS
                    (+/LS
                     (*/LS (*/LS (*/LS (*/LS i 6) 4) 3) 6)
                     (*/LS (*/LS (-/LS j2 1) 6) 3))
                    (+/LS k2 1))))
                 (wrk2
                  (+/LS
                   (+/LS
                    (*/LS (*/LS (*/LS (*/LS i 6) 4) 3) 6)
                    (*/LS (*/LS (-/LS j2 1) 6) 3))
                   (-/LS k2 1))))
                (wrk2
                 (+/LS
                  (+/LS
                   (*/LS (*/LS (*/LS (*/LS i 6) 4) 3) 6)
                   (*/LS (*/LS (+/LS j2 1) 6) 3))
                  (-/LS k2 1))))))
             (*/LS
              (b2 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
              (+/LS
               (-/LS
                (-/LS
                 (wrk2
                  (+/LS
                   (+/LS
                    (*/LS (*/LS (*/LS (*/LS (+/LS i 1) 6) 4) 3) 6)
                    (*/LS (*/LS j2 6) 3))
                   (+/LS k2 1)))
                 (wrk2
                  (+/LS
                   (+/LS
                    (*/LS (*/LS (*/LS (*/LS (-/LS i 1) 6) 4) 3) 6)
                    (*/LS (*/LS j2 6) 3))
                   (+/LS k2 1))))
                (wrk2
                 (+/LS
                  (+/LS
                   (*/LS (*/LS (*/LS (*/LS (+/LS i 1) 6) 4) 3) 6)
                   (*/LS (*/LS j2 6) 3))
                  (-/LS k2 1))))
               (wrk2
                (+/LS
                 (+/LS
                  (*/LS (*/LS (*/LS (*/LS (-/LS i 1) 6) 4) 3) 6)
                  (*/LS (*/LS j2 6) 3))
                 (-/LS k2 1))))))
            (*/LS
             (c0 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
             (wrk2
              (+/LS
               (+/LS
                (*/LS (*/LS (*/LS (*/LS (-/LS i 1) 6) 4) 3) 6)
                (*/LS (*/LS j2 6) 3))
               k2))))
           (*/LS
            (c1 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
            (wrk2
             (+/LS
              (+/LS
               (*/LS (*/LS (*/LS (*/LS i 6) 4) 3) 6)
               (*/LS (*/LS (-/LS j2 1) 6) 3))
              k2))))
          (*/LS
           (c2 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
           (wrk2
            (+/LS
             (+/LS (*/LS (*/LS (*/LS (*/LS i 6) 4) 3) 6) (*/LS (*/LS j2 6) 3))
             (-/LS k2 1)))))
         (wrk1 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))))
       (=
        ss
        (*/LS
         (-/LS
          (*/LS s0 (a3 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k)))
          (wrk2
           (+/LS
            (+/LS (*/LS (*/LS (*/LS (*/LS i 6) 4) 3) 6) (*/LS (*/LS j2 6) 3))
            k2)))
         (bnd (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))))
       (= temp (+/LS temp (*/LS ss ss)))
       (=
        (p (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
        (+/LS
         (wrk2
          (+/LS
           (+/LS (*/LS (*/LS (*/LS (*/LS i 6) 4) 3) 6) (*/LS (*/LS j2 6) 3))
           k2))
         (*/LS omega ss)))))
     (+=/LS c xy)))
  (syncthreads)
  (= (gosa tid) temp))
(define (main)
  (begin (define i 0) (define j 0) (define k 0))
  (begin (define final_gosa 0))
  (begin
    (define cpu0 0)
    (define cpu1 0)
    (define nflop 0)
    (define xmflops2 0)
    (define score 0))
  (begin (: float (gosa (* (* (* 6 4) 3) 6))))
  (begin (:* float p))
  (begin (:* float a0) (:* float a1) (:* float a2) (:* float a3))
  (begin (:* float b0) (:* float b1) (:* float b2))
  (begin (:* float c0) (:* float c1) (:* float c2))
  (begin (:* float bnd))
  (begin (:* float wrk1) (:* float wrk2))
  (set! mimax 4)
  (set! mjmax (+ (* 6 (- 4 2)) 2))
  (set! mkmax (+ (* 3 (- 6 2)) 2))
  (set! imax (- 4 1))
  (set! jmax (- (+ (* 6 (- 4 2)) 2) 1))
  (set! kmax (- (+ (* 3 (- 6 2)) 2) 1))
  (begin (define N_IJK (* (* mimax mjmax) mkmax)))
  (begin (define WORKSIZE (* (* (* (* 6 4) 3) 6) mimax)))
  (begin (:* float dev_p))
  (begin
    (:* float dev_a0)
    (:* float dev_a1)
    (:* float dev_a2)
    (:* float dev_a3))
  (begin (:* float dev_b0) (:* float dev_b1) (:* float dev_b2))
  (begin (:* float dev_c0) (:* float dev_c1) (:* float dev_c2))
  (begin (:* float dev_bnd))
  (begin (:* float dev_wrk1) (:* float dev_wrk2))
  (begin (:* float dev_gosa))
  (set! omega 0.8)
  (set! a0 (malloc (* 1 N_IJK)))
  (set! a1 (malloc (* 1 N_IJK)))
  (set! a2 (malloc (* 1 N_IJK)))
  (set! a3 (malloc (* 1 N_IJK)))
  (set! b0 (malloc (* 1 N_IJK)))
  (set! b1 (malloc (* 1 N_IJK)))
  (set! b2 (malloc (* 1 N_IJK)))
  (set! c0 (malloc (* 1 N_IJK)))
  (set! c1 (malloc (* 1 N_IJK)))
  (set! c2 (malloc (* 1 N_IJK)))
  (set! p (malloc (* 1 N_IJK)))
  (set! wrk1 (malloc (* 1 N_IJK)))
  (set! wrk2 (malloc (* 1 WORKSIZE)))
  (set! bnd (malloc (* 1 N_IJK)))
  (cudaMalloc dev_a0 (* N_IJK 1))
  (cudaMalloc dev_a1 (* N_IJK 1))
  (cudaMalloc dev_a2 (* N_IJK 1))
  (cudaMalloc dev_a3 (* N_IJK 1))
  (cudaMalloc dev_b0 (* N_IJK 1))
  (cudaMalloc dev_b1 (* N_IJK 1))
  (cudaMalloc dev_b2 (* N_IJK 1))
  (cudaMalloc dev_c0 (* N_IJK 1))
  (cudaMalloc dev_c1 (* N_IJK 1))
  (cudaMalloc dev_c2 (* N_IJK 1))
  (cudaMalloc dev_p (* N_IJK 1))
  (cudaMalloc dev_bnd (* N_IJK 1))
  (cudaMalloc dev_wrk1 (* N_IJK 1))
  (cudaMalloc dev_wrk2 (* WORKSIZE 1))
  (cudaMalloc dev_gosa (* (* (* (* 1 6) 4) 3) 6))
  (for-
   ((set! i 0) : (< i mimax) : (++ i))
   (for-
    ((set! j 0) : (< j mjmax) : (++ j))
    (for-
     ((set! k 0) : (< k mkmax) : (++ k))
     (array-set-host! a0 (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k) 1.0)
     (array-set-host! a1 (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k) 1.0)
     (array-set-host! a2 (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k) 1.0)
     (array-set-host!
      a3
      (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k)
      (/ 1.0 6.0))
     (array-set-host! b0 (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k) 0.0)
     (array-set-host! b1 (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k) 0.0)
     (array-set-host! b2 (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k) 0.0)
     (array-set-host! c0 (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k) 1.0)
     (array-set-host! c1 (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k) 1.0)
     (array-set-host! c2 (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k) 1.0)
     (array-set-host!
      p
      (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k)
      (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k))
     ;(/ (* i i) (* imax imax)))
     (array-set-host! wrk1 (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k) 0.0)
     (array-set-host! bnd (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k) 1.0))))
  (for ([i WORKSIZE])
    (array-set-host! wrk2 i 0.0))
  (for ([i (* (* (* (* 1 6) 4) 3) 6)])
    (array-set-host! dev_gosa i 0.0))
  (cudaMemcpy dev_a0 a0 (* N_IJK 1) 1)
  (cudaMemcpy dev_a1 a1 (* N_IJK 1) 1)
  (cudaMemcpy dev_a2 a2 (* N_IJK 1) 1)
  (cudaMemcpy dev_a3 a3 (* N_IJK 1) 1)
  (cudaMemcpy dev_b0 b0 (* N_IJK 1) 1)
  (cudaMemcpy dev_b1 b1 (* N_IJK 1) 1)
  (cudaMemcpy dev_b2 b2 (* N_IJK 1) 1)
  (cudaMemcpy dev_c0 c0 (* N_IJK 1) 1)
  (cudaMemcpy dev_c1 c1 (* N_IJK 1) 1)
  (cudaMemcpy dev_c2 c2 (* N_IJK 1) 1)
  (cudaMemcpy dev_wrk1 wrk1 (* N_IJK 1) 1)
  (cudaMemcpy dev_wrk2 wrk2 (* WORKSIZE 1) 1)
  (cudaMemcpy dev_bnd bnd (* N_IJK 1) 1)
  (cudaMemcpy dev_p p (* N_IJK 1) 1)
  ;(set! cpu0 (second))
  (for ([i N_IJK])
    (println (array-ref-host dev_wrk1 i)))
  (define block (list 6 4))
  (define grid (list 3 6))
  (invoke-kernel
   jacobi
   grid
   block
   dev_a0
   dev_a1
   dev_a2
   dev_a3
   dev_b0
   dev_b1
   dev_b2
   dev_c0
   dev_c1
   dev_c2
   dev_p
   dev_wrk1
   dev_wrk2
   dev_bnd
   3
   mimax
   mjmax
   mkmax
   omega
   dev_gosa)
  (cudaDeviceSynchronize)
  ;(set! cpu1 (second))
  (cudaMemcpy gosa dev_gosa (* (* (* (* 1 6) 4) 3) 6) 0)
  (cudaFree dev_a0)
  (cudaFree dev_a1)
  (cudaFree dev_a2)
  (cudaFree dev_a3)
  (cudaFree dev_b0)
  (cudaFree dev_b1)
  (cudaFree dev_b2)
  (cudaFree dev_c0)
  (cudaFree dev_c1)
  (cudaFree dev_c2)
  (cudaFree dev_p)
  (cudaFree dev_wrk1)
  (cudaFree dev_wrk2)
  (cudaFree dev_bnd)
  (cudaFree dev_gosa)
  (for-
   ((begin (define gosa_index 0))
    :
    (< gosa_index (* (* (* 6 4) 3) 6))
    :
    (++ gosa_index))
   (+= final_gosa (puts (array-ref-host gosa gosa_index))))
  (set! nflop (* (* (* (- kmax 2) (- jmax 2)) (- imax 2)) 34))
  ;(if (!= cpu1 0.0) ((set! xmflops2 (* (* (/ nflop cpu1) 1e-06) 3))))
  (set! score (/ xmflops2 32.27))
  final_gosa)
(time 
 (main))

(close-output-port out-file)