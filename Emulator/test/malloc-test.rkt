#lang rosette

(require rackunit
         "../lang.rkt")

(define BLOCKSIZE 32)

(define (main1)
  (begin (:* int d_arr))
  (begin (: int (arr 32)))
  (cudaMalloc (&/LS d_arr) 32)
  ;(kernel 1 1 d_hello)
  (cudaMemcpy arr d_arr 32 "cudaMemcpyDeviceToHost")
  (cudaFree d_arr)
  (print "hello"))

(main1)

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
  (begin (: int i) (: int j) (: int k) (: int n))
  (begin (: float s0) (: float ss) (: float temp))
  (begin (:= int tid (thread-idx 0)))
  (begin (:= int size (//LS (-/LS imax 1) (-/LS imax 1))))
  (for-
   ((= n 0) : (</LS n nn) : (++/LS n))
   ((= temp 0.0)
    (for-
     ((= i (*/LS tid size)) : (</LS i (*/LS (+/LS tid 1) size)) : (++/LS i))
     (for-
      ((= j 1) : (</LS j (-/LS jmax 1)) : (++/LS j))
      (for-
       ((= k 1) : (</LS k (-/LS kmax 1)) : (++/LS k))
       ((=
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
                   (a0
                    (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
                   (p
                    (+/LS
                     (+/LS (*/LS (*/LS (+/LS i 1) jmax) kmax) (*/LS j kmax))
                     k)))
                  (*/LS
                   (a1
                    (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
                   (p
                    (+/LS
                     (+/LS (*/LS (*/LS i jmax) kmax) (*/LS (+/LS j 1) kmax))
                     k))))
                 (*/LS
                  (a2
                   (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
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
                    (+/LS (*/LS (*/LS i jmax) kmax) (*/LS (+/LS j 1) kmax))
                    (-/LS k 1))))
                 (p
                  (+/LS
                   (+/LS (*/LS (*/LS i jmax) kmax) (*/LS (-/LS j 1) kmax))
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
              (+/LS
               (+/LS (*/LS (*/LS i jmax) kmax) (*/LS (-/LS j 1) kmax))
               k))))
           (*/LS
            (c2 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
            (p
             (+/LS
              (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax))
              (-/LS k 1)))))
          (wrk1 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))))
        (=
         ss
         (*/LS
          (-/LS
           (*/LS
            s0
            (a3 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k)))
           (p (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k)))
          (bnd (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))))
        (= temp (+/LS temp (*/LS ss ss)))
        (=
         (wrk2 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
         (+/LS
          (p (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
          (*/LS omega ss)))))))
    (for-
     ((= i (*/LS tid size)) : (</LS i (*/LS (+/LS tid 1) size)) : (++/LS i))
     (for-
      ((= j 1) : (</LS j (-/LS jmax 1)) : (++/LS j))
      (for-
       ((= k 1) : (</LS k (-/LS kmax 1)) : (++/LS k))
       (=
        (p (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
        (wrk2 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))))))))
  (= (gosa tid) temp))


(define (main)
  (begin (define imax 0) (define jmax 0) (define kmax 0))
  (begin (define omega 0))
  (begin (define i 0) (define j 0) (define k 0))
  (begin (define final_gosa 0))
  (begin
    (define cpu0 0)
    (define cpu1 0)
    (define nflop 0)
    (define xmflops2 0)
    (define score 0))
  (begin (: float (gosa 127)))
  (begin (:* float p))
  (begin (:* float a0) (:* float a1) (:* float a2) (:* float a3))
  (begin (:* float b0) (:* float b1) (:* float b2))
  (begin (:* float c0) (:* float c1) (:* float c2))
  (begin (:* float bnd))
  (begin (:* float wrk1) (:* float wrk2))
  (set! imax (- 129 1))
  (set! jmax (- 65 1))
  (set! kmax (- 65 1))
  (begin (define N_IJK (* (* imax jmax) kmax)))
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
  (set! wrk2 (malloc (* 1 N_IJK)))
  (set! bnd (malloc (* 1 N_IJK)))
  (println "check")
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
  (cudaMalloc dev_wrk2 (* N_IJK 1))
  (cudaMalloc dev_gosa (* 1 127))
  (println "malloc passed")
  (for-
   ((set! i 0) : (< i imax) : (++ i))
   (for-
    ((set! j 0) : (< j jmax) : (++ j))
    (for-
     ((set! k 0) : (< k kmax) : (++ k))
     (array-set-host! a0 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
     (array-set-host! a1 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
     (array-set-host! a2 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
     (array-set-host! a3 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
     (array-set-host! b0 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
     (array-set-host! b1 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
     (array-set-host! b2 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
     (array-set-host! c0 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
     (array-set-host! c1 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
     (array-set-host! c2 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
     (array-set-host! p (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
     (array-set-host! wrk1 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
     (array-set-host! bnd (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0))))
  (for-
   ((set! i 0) : (< i imax) : (++ i))
   (for-
    ((set! j 0) : (< j jmax) : (++ j))
    (for-
     ((set! k 0) : (< k kmax) : (++ k))
     (array-set-host! a0 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 1.0)
     (array-set-host! a1 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 1.0)
     (array-set-host! a2 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 1.0)
     (array-set-host!
      a3
      (+ (+ (* (* i jmax) kmax) (* j kmax)) k)
      (/ 1.0 6.0))
     (array-set-host! b0 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
     (array-set-host! b1 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
     (array-set-host! b2 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
     (array-set-host! c0 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 1.0)
     (array-set-host! c1 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 1.0)
     (array-set-host! c2 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 1.0)
     (array-set-host!
      p
      (+ (+ (* (* i jmax) kmax) (* j kmax)) k)
      (/ (* k k) (* (- kmax 1) (- kmax 1))))
     (array-set-host! wrk1 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
     (array-set-host! bnd (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 1.0))))
  (cudaMemcpy dev_a0 a0 (* N_IJK 1) "cudaMemcpyHosttoDevice")
  (cudaMemcpy dev_a1 a1 (* N_IJK 1) "cudaMemcpyHosttoDevice")
  (cudaMemcpy dev_a2 a2 (* N_IJK 1) "cudaMemcpyHosttoDevice")
  (cudaMemcpy dev_a3 a3 (* N_IJK 1) "cudaMemcpyHosttoDevice")
  (cudaMemcpy dev_b0 b0 (* N_IJK 1) "cudaMemcpyHosttoDevice")
  (cudaMemcpy dev_b1 b1 (* N_IJK 1) "cudaMemcpyHosttoDevice")
  (cudaMemcpy dev_b2 b2 (* N_IJK 1) "cudaMemcpyHosttoDevice")
  (cudaMemcpy dev_c0 c0 (* N_IJK 1) "cudaMemcpyHosttoDevice")
  (cudaMemcpy dev_c1 c1 (* N_IJK 1) "cudaMemcpyHosttoDevice")
  (cudaMemcpy dev_c2 c2 (* N_IJK 1) "cudaMemcpyHosttoDevice")
  (cudaMemcpy dev_wrk1 wrk1 (* N_IJK 1) "cudaMemcpyHosttoDevice")
  (cudaMemcpy dev_wrk2 wrk2 (* N_IJK 1) "cudaMemcpyHosttoDevice")
  (cudaMemcpy dev_bnd bnd (* N_IJK 1) "cudaMemcpyHosttoDevice")
  (cudaMemcpy dev_p p (* N_IJK 1) "cudaMemcpyHosttoDevice")
  ;  (invoke-kernel
  ;   jacobi
  ;   '(1)
  ;   '(127)
  ;   dev_a0
  ;   dev_a1
  ;   dev_a2
  ;   dev_a3
  ;   dev_b0
  ;   dev_b1
  ;   dev_b2
  ;   dev_c0
  ;   dev_c1
  ;   dev_c2
  ;   dev_p
  ;   dev_wrk1
  ;   dev_wrk2
  ;   dev_bnd
  ;   200
  ;   imax
  ;   jmax
  ;   kmax
  ;   omega
  ;   dev_gosa)
  (cudaMemcpy gosa dev_gosa (* 1 127) "cudaMemcpyDeviceToHost")
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
   ((begin (define gosa_index 0)) : (< gosa_index 127) : (+= gosa_index 1))
   ((+= final_gosa (array-ref-host gosa gosa_index))))
  (set! nflop (* (* (* (- kmax 2) (- jmax 2)) (- imax 2)) 34))
  0)


(main)