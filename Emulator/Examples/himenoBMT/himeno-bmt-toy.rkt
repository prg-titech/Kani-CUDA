#lang rosette

(require rackunit
         "../../lang.rkt")

(provide gosa-spec)

(define out-file (open-output-file "profile" #:exists 'truncate))
(fprintf out-file "tid bid id smid threadIdx.x threadIdx.y blockDim.x blockDim.y csb BLOCKSIZE\n")

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
    (: int n)
    (: int xy)
    (: int c)
    (: int csb))
  (begin (: float s0) (: float ss) (: float temp))
  (define BLOCKSIZE (* (+ (block-dim 0) 2) (+ (block-dim 1) 2)))
  (= k (+/LS (+/LS (thread-idx 0) (*/LS (block-dim 0) (block-idx 0))) 1))
  (= j (+/LS (+/LS (thread-idx 1) (*/LS (block-dim 1) (block-idx 1))) 1))
  (begin (:= int tid (+/LS (-/LS k 1) (*/LS (-/LS j 1) (-/LS kmax 2)))))
  (= xy (*/LS kmax jmax))
  (begin (:shared float (sb (* 3 BLOCKSIZE))))
  (= csb (+/LS (thread-idx 0) 1 (*/LS (+/LS (thread-idx 1) 1) (+/LS (block-dim 0) 2))))
  (for-
   ((= n 0) : (</LS n nn) : (++/LS n))
   (= c (+/LS (*/LS j kmax) k))
   (= temp 0.0)
   (= (sb (+/LS csb BLOCKSIZE)) (p c))
   (= (sb (+/LS csb (*/LS 2 BLOCKSIZE))) (p (+/LS c xy)))
   (for-
    ((= i 1) : (</LS i (-/LS imax 1)) : (++/LS i)) 
    (+=/LS c xy)
    (= (sb csb) (sb (+/LS csb BLOCKSIZE)))
    (= (sb (+/LS csb BLOCKSIZE)) (sb (+/LS csb (*/LS 2 BLOCKSIZE))))
    (= (sb (+/LS csb (*/LS 2 BLOCKSIZE))) (p (+/LS c xy)))
    (syncthreads)
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
               (profiling-access3
                out-file
                p
                (+/LS
                 (+/LS (*/LS (*/LS (-/LS i 1) jmax) kmax) (*/LS (-/LS j 1) kmax))
                 k 1)
                sb
                (-/LS csb (block-dim 0) 2 -1)
                ;;tid bid id smid threadIdx.x threadIdx.y blockDim.x blockDim.y csb BLOCKSIZE
                (thread-idx 0) (thread-idx 1) (block-dim 0) (block-dim 1) csb BLOCKSIZE))
              ;               (profiling-access
              ;                out-file
              ;                p
              ;                (+/LS
              ;                 (+/LS (*/LS (*/LS (-/LS i 1) jmax) kmax) (*/LS (-/LS j 1) kmax))
              ;                 k 1)
              ;                ;;tid bid id smid threadIdx.x threadIdx.y blockDim.x blockDim.y csb BLOCKSIZE
              ;                (thread-idx 0) (thread-idx 1) (block-dim 0) (block-dim 1) csb BLOCKSIZE))
              (*/LS
               (a1 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
               (profiling-access3
                out-file
                p
                (+/LS
                 (+/LS (*/LS (*/LS (-/LS i 1) jmax) kmax) (*/LS (-/LS j 1) kmax))
                 k 1)
                sb
                (-/LS csb (block-dim 0) 2 -1)
                ;;tid bid id smid threadIdx.x threadIdx.y blockDim.x blockDim.y csb BLOCKSIZE
                (thread-idx 0) (thread-idx 1) (block-dim 0) (block-dim 1) csb BLOCKSIZE)))
             (*/LS
              (a2 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
              (profiling-access3
               out-file
               p
               (+/LS
                (+/LS (*/LS (*/LS (-/LS i 1) jmax) kmax) (*/LS (-/LS j 1) kmax))
                k 1)
               sb
               (-/LS csb (block-dim 0) 2 -1)
               ;;tid bid id smid threadIdx.x threadIdx.y blockDim.x blockDim.y csb BLOCKSIZE
               (thread-idx 0) (thread-idx 1) (block-dim 0) (block-dim 1) csb BLOCKSIZE)))
            (*/LS
             (b0 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
             (+/LS
              (-/LS
               (-/LS
                (profiling-access3
                 out-file
                 p
                 (+/LS
                  (+/LS (*/LS (*/LS (-/LS i 1) jmax) kmax) (*/LS (-/LS j 1) kmax))
                  k 1)
                 sb
                 (-/LS csb (block-dim 0) 2 -1)
                 ;;tid bid id smid threadIdx.x threadIdx.y blockDim.x blockDim.y csb BLOCKSIZE
                 (thread-idx 0) (thread-idx 1) (block-dim 0) (block-dim 1) csb BLOCKSIZE)
                (profiling-access3
                 out-file
                 p
                 (+/LS
                  (+/LS (*/LS (*/LS (-/LS i 1) jmax) kmax) (*/LS (-/LS j 1) kmax))
                  k 1)
                 sb
                 (-/LS csb (block-dim 0) 2 -1)
                 ;;tid bid id smid threadIdx.x threadIdx.y blockDim.x blockDim.y csb BLOCKSIZE
                 (thread-idx 0) (thread-idx 1) (block-dim 0) (block-dim 1) csb BLOCKSIZE))
               (profiling-access3
                out-file
                p
                (+/LS
                 (+/LS (*/LS (*/LS (-/LS i 1) jmax) kmax) (*/LS (-/LS j 1) kmax))
                 k 1)
                sb
                (-/LS csb (block-dim 0) 2 -1)
                ;;tid bid id smid threadIdx.x threadIdx.y blockDim.x blockDim.y csb BLOCKSIZE
                (thread-idx 0) (thread-idx 1) (block-dim 0) (block-dim 1) csb BLOCKSIZE))
              (profiling-access3
               out-file
               p
               (+/LS
                (+/LS (*/LS (*/LS (-/LS i 1) jmax) kmax) (*/LS (-/LS j 1) kmax))
                k 1)
               sb
               (-/LS csb (block-dim 0) 2 -1)
               ;;tid bid id smid threadIdx.x threadIdx.y blockDim.x blockDim.y csb BLOCKSIZE
               (thread-idx 0) (thread-idx 1) (block-dim 0) (block-dim 1) csb BLOCKSIZE))))
           (*/LS
            (b1 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
            (+/LS
             (-/LS
              (-/LS
               (profiling-access3
                out-file
                p
                (+/LS
                 (+/LS (*/LS (*/LS (-/LS i 1) jmax) kmax) (*/LS (-/LS j 1) kmax))
                 k 1)
                sb
                (-/LS csb (block-dim 0) 2 -1)
                ;;tid bid id smid threadIdx.x threadIdx.y blockDim.x blockDim.y csb BLOCKSIZE
                (thread-idx 0) (thread-idx 1) (block-dim 0) (block-dim 1) csb BLOCKSIZE)
               (profiling-access3
                out-file
                p
                (+/LS
                 (+/LS (*/LS (*/LS (-/LS i 1) jmax) kmax) (*/LS (-/LS j 1) kmax))
                 k 1)
                sb
                (-/LS csb (block-dim 0) 2 -1)
                ;;tid bid id smid threadIdx.x threadIdx.y blockDim.x blockDim.y csb BLOCKSIZE
                (thread-idx 0) (thread-idx 1) (block-dim 0) (block-dim 1) csb BLOCKSIZE))
              (profiling-access3
               out-file
               p
               (+/LS
                (+/LS (*/LS (*/LS (-/LS i 1) jmax) kmax) (*/LS (-/LS j 1) kmax))
                k 1)
               sb
               (-/LS csb (block-dim 0) 2 -1)
               ;;tid bid id smid threadIdx.x threadIdx.y blockDim.x blockDim.y csb BLOCKSIZE
               (thread-idx 0) (thread-idx 1) (block-dim 0) (block-dim 1) csb BLOCKSIZE))
             (profiling-access3
              out-file
              p
              (+/LS
               (+/LS (*/LS (*/LS (-/LS i 1) jmax) kmax) (*/LS (-/LS j 1) kmax))
               k 1)
              sb
              (-/LS csb (block-dim 0) 2 -1)
              ;;tid bid id smid threadIdx.x threadIdx.y blockDim.x blockDim.y csb BLOCKSIZE
              (thread-idx 0) (thread-idx 1) (block-dim 0) (block-dim 1) csb BLOCKSIZE))))
          (*/LS
           (b2 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
           (+/LS
            (-/LS
             (-/LS
              (profiling-access3
               out-file
               p
               (+/LS
                (+/LS (*/LS (*/LS (-/LS i 1) jmax) kmax) (*/LS (-/LS j 1) kmax))
                k 1)
               sb
               (-/LS csb (block-dim 0) 2 -1)
               ;;tid bid id smid threadIdx.x threadIdx.y blockDim.x blockDim.y csb BLOCKSIZE
               (thread-idx 0) (thread-idx 1) (block-dim 0) (block-dim 1) csb BLOCKSIZE)
              (profiling-access3
               out-file
               p
               (+/LS
                (+/LS (*/LS (*/LS (-/LS i 1) jmax) kmax) (*/LS (-/LS j 1) kmax))
                k 1)
               sb
               (-/LS csb (block-dim 0) 2 -1)
               ;;tid bid id smid threadIdx.x threadIdx.y blockDim.x blockDim.y csb BLOCKSIZE
               (thread-idx 0) (thread-idx 1) (block-dim 0) (block-dim 1) csb BLOCKSIZE))
             (profiling-access3
              out-file
              p
              (+/LS
               (+/LS (*/LS (*/LS (-/LS i 1) jmax) kmax) (*/LS (-/LS j 1) kmax))
               k 1)
              sb
              (-/LS csb (block-dim 0) 2 -1)
              ;;tid bid id smid threadIdx.x threadIdx.y blockDim.x blockDim.y csb BLOCKSIZE
              (thread-idx 0) (thread-idx 1) (block-dim 0) (block-dim 1) csb BLOCKSIZE))
            (profiling-access3
             out-file
             p
             (+/LS
              (+/LS (*/LS (*/LS (-/LS i 1) jmax) kmax) (*/LS (-/LS j 1) kmax))
              k 1)
             sb
             (-/LS csb (block-dim 0) 2 -1)
             ;;tid bid id smid threadIdx.x threadIdx.y blockDim.x blockDim.y csb BLOCKSIZE
             (thread-idx 0) (thread-idx 1) (block-dim 0) (block-dim 1) csb BLOCKSIZE))))
         (*/LS
          (c0 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
          (profiling-access3
           out-file
           p
           (+/LS
            (+/LS (*/LS (*/LS (-/LS i 1) jmax) kmax) (*/LS (-/LS j 1) kmax))
            k 1)
           sb
           (-/LS csb (block-dim 0) 2 -1)
           ;;tid bid id smid threadIdx.x threadIdx.y blockDim.x blockDim.y csb BLOCKSIZE
           (thread-idx 0) (thread-idx 1) (block-dim 0) (block-dim 1) csb BLOCKSIZE)))
        (*/LS
         (c1 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
         (profiling-access3
          out-file
          p
          (+/LS
           (+/LS (*/LS (*/LS (-/LS i 1) jmax) kmax) (*/LS (-/LS j 1) kmax))
           k 1)
          sb
          (-/LS csb (block-dim 0) 2 -1)
          ;;tid bid id smid threadIdx.x threadIdx.y blockDim.x blockDim.y csb BLOCKSIZE
          (thread-idx 0) (thread-idx 1) (block-dim 0) (block-dim 1) csb BLOCKSIZE)))
       (*/LS
        (c2 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
        (profiling-access3
         out-file
         p
         (+/LS
          (+/LS (*/LS (*/LS (-/LS i 1) jmax) kmax) (*/LS (-/LS j 1) kmax))
          k 1)
         sb
         (-/LS csb (block-dim 0) 2 -1)
         ;;tid bid id smid threadIdx.x threadIdx.y blockDim.x blockDim.y csb BLOCKSIZE
         (thread-idx 0) (thread-idx 1) (block-dim 0) (block-dim 1) csb BLOCKSIZE)))
      (wrk1 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))))
    (=
     ss
     (*/LS
      (-/LS
       (*/LS
        s0
        (a3 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k)))
       (profiling-access3
        out-file
        p
        (+/LS
         (+/LS (*/LS (*/LS (-/LS i 1) jmax) kmax) (*/LS (-/LS j 1) kmax))
         k 1)
        sb
        (-/LS csb (block-dim 0) 2 -1)
        ;;tid bid id smid threadIdx.x threadIdx.y blockDim.x blockDim.y csb BLOCKSIZE
        (thread-idx 0) (thread-idx 1) (block-dim 0) (block-dim 1) csb BLOCKSIZE))
      (bnd (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))))
    (= temp (+/LS temp (*/LS ss ss)))
    (=
     (wrk2 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
     (+/LS
      (profiling-access3
       out-file
       p
       (+/LS
        (+/LS (*/LS (*/LS (-/LS i 1) jmax) kmax) (*/LS (-/LS j 1) kmax))
        k 1)
       sb
       (-/LS csb (block-dim 0) 2 -1)
       ;;tid bid id smid threadIdx.x threadIdx.y blockDim.x blockDim.y csb BLOCKSIZE
       (thread-idx 0) (thread-idx 1) (block-dim 0) (block-dim 1) csb BLOCKSIZE)
      (*/LS omega ss))))
   (syncthreads)
   (for-
    ((= i 1) : (</LS i (-/LS imax 1)) : (++/LS i))
    (=
     (p (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k))
     (wrk2 (+/LS (+/LS (*/LS (*/LS i jmax) kmax) (*/LS j kmax)) k)))))
  (= (gosa tid) temp))

(define (main)
  ;  (writeln "Declarate...")
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
  (begin (:* float p))
  (begin (:* float a0) (:* float a1) (:* float a2) (:* float a3))
  (begin (:* float b0) (:* float b1) (:* float b2))
  (begin (:* float c0) (:* float c1) (:* float c2))
  (begin (:* float bnd))
  (begin (:* float wrk1) (:* float wrk2))
  (define BLOCKSIZEX 4)
  (define BLOCKSIZEY 3)
  (define GRIDSIZEX 4)
  (define GRIDSIZEY 4)
  (begin (define BLOCKSIZE (* BLOCKSIZEX BLOCKSIZEY)))
  (begin (define GRIDSIZE (* GRIDSIZEX GRIDSIZEY)))
  (begin (define THREADNUM (* GRIDSIZE BLOCKSIZE)))
  (define mkmax (+ (* GRIDSIZEX BLOCKSIZEX) 2))
  (define mjmax (+ (* GRIDSIZEY BLOCKSIZEY) 2))
  (define mimax 3)
  (set! imax (- mimax 1))
  (set! jmax (- mjmax 1))
  (set! kmax (- mkmax 1))
  (begin (define N_IJK (puts (* (* mimax mjmax) mkmax))))
  (begin (: float (gosa THREADNUM)))
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
  ;  (writeln "Passed")
  ;  (writeln "malloc...")
  ;  (time
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
  (cudaMalloc dev_gosa (* 1 THREADNUM))
  ;  (writeln "Passeded")
  ;  (writeln "Initialize...")
  ;  (time
  ;  (for-
  ;   ((set! i 0) : (< i imax) : (++ i))
  ;   (for-
  ;    ((set! j 0) : (< j jmax) : (++ j))
  ;    (for-
  ;     ((set! k 0) : (< k kmax) : (++ k))
  ;     (array-set-host! a0 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
  ;     (array-set-host! a1 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
  ;     (array-set-host! a2 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
  ;     (array-set-host! a3 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
  ;     (array-set-host! b0 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
  ;     (array-set-host! b1 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
  ;     (array-set-host! b2 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
  ;     (array-set-host! c0 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
  ;     (array-set-host! c1 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
  ;     (array-set-host! c2 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
  ;     (array-set-host! p (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
  ;     (array-set-host! wrk1 (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0)
  ;     (array-set-host! bnd (+ (+ (* (* i jmax) kmax) (* j kmax)) k) 0.0))))
  (for-
   ((set! i 0) : (< i mimax) : (++ i))
   (for-
    ((set! j 0) : (< j mjmax) : (++ j))
    (for-
     ((set! k 0) : (< k mkmax) : (++ k))
     (array-set-host! a0 (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k) 1.0)
     (array-set-host! a1 (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k) 1.0)
     (array-set-host! a2 (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k) 1.0)
     (array-set-host! a3 (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k) (/ 1.0 6.0))
     (array-set-host! b0 (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k) 0.0)
     (array-set-host! b1 (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k) 0.0)
     (array-set-host! b2 (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k) 0.0)
     (array-set-host! c0 (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k) 1.0)
     (array-set-host! c1 (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k) 1.0)
     (array-set-host! c2 (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k) 1.0)
     (array-set-host! p (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k) (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k));(/ (* k k) (* (- mkmax 1) (- mkmax 1))))
     (array-set-host! wrk1 (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k) 0.0)
     (array-set-host! bnd (+ (+ (* (* i mjmax) mkmax) (* j mkmax)) k) 1.0))))
  ;(print-matrix p N_IJK 1)
  ;  (writeln "Passed")
  ;  (writeln "Copy from host to device...")
  ;  (time
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
  ;  (writeln "Passed")
  ;  (writeln "Execute kernel...")
  ;(print-matrix dev_p N_IJK 1)
  (invoke-kernel jacobi (list GRIDSIZEX GRIDSIZEY) (list BLOCKSIZEX BLOCKSIZEY) dev_a0 dev_a1 dev_a2 dev_a3 dev_b0 dev_b1 dev_b2 dev_c0 dev_c1 dev_c2 dev_p dev_wrk1 dev_wrk2 dev_bnd 4 mimax mjmax mkmax omega dev_gosa)
  ;  (writeln "Passeded")
  (cudaMemcpy gosa dev_gosa (* 1 THREADNUM) "cudaMemcpyDeviceToHost")
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
   ((begin (define gosa_index 0)) : (< gosa_index THREADNUM) : (+= gosa_index 1))
   (+= final_gosa (array-ref-host gosa gosa_index)))
  (set! nflop (* (* (* (- kmax 2) (- jmax 2)) (- imax 2)) 34))
  (println nflop)
  ;(print-matrix gosa THREADNUM 1)
  final_gosa
  gosa)

(define gosa-spec
  (time 
   (main)))

(close-output-port out-file)
