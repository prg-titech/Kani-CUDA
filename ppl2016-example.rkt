#lang rosette

;; An example from "GPGPU向けデータ並列コードテンプレートの形式検証"
;; http://prg.is.titech.ac.jp/wp-content/uploads/2016/02/ppl.pdf

(require "lang.rkt")

;; number of iteration, min{ 2^n | ntid <= 2^n}
(define (n-iter ntid)
  (define (n-iter-aux n)
    (if (< n ntid) (n-iter-aux (* 2 n)) n))
  (n-iter-aux 1))

(define (reduce arr len nb)
  (:= int s (quotient/LS nb 2))
  (: int x0 x1 x2)
  (= x1 [arr (tid)])
  (while (</LS 0 s) 
    (barrier)
    (if- (</LS (+/LS (tid) s) len)
         (begin 
           (= x0 [arr (+/LS (tid) s)])
           (= x2 (+/LS x1 x0))
           (= x1 x2)))
    (barrier)
    (= [arr (tid)] x1)
    (= s (quotient/LS s 2))))

(define (reduce-opt arr len nb)
  (:= int s (quotient/LS nb 2))
  (: int x0 x1 x2)
  (= x1 [arr (tid)])
  (while (</LS 0 s) 
    (barrier)
    (if- (&&/LS (</LS (+/LS (tid) s) len)
                (</LS (tid) s))
         (begin 
           (= x0 [arr (+/LS (tid) s)])
           (= x2 (+/LS x1 x0))
           (= x1 x2)
           (= [arr (tid)] x1)))
    (= s (quotient/LS s 2))))

(define (reduce-opt-sketch arr len nb)
  (:= int s (quotient/LS nb 2))
  (: int x0 x1 x2)
  (= x1 [arr (tid)])
  (while (</LS 0 s) 
    (choose (barrier) (void))
    (if- (&&/LS (</LS (+/LS (tid) s) len)
                (</LS (tid) s))
         (begin 
           (= x0 [arr (+/LS (tid) s)])
           (= x2 (+/LS x1 x0))
           (= x1 x2)
           (= [arr (tid)] x1)))
    (choose (barrier) (void))
    (= s (quotient/LS s 2))))

(define (create-array size)
  (make-array
   (for/vector ([i (in-range size)])
     (make-element 1))))

;; make two symbolic arrays whose contents are equal
(define (create-sym-arrays size)
  (let ([xs (for/vector ([i (in-range size)])
             (define-symbolic* i integer?)
             (make-element i))])
    (define ys (make-vector size #f) )
    (for ([i (in-range size)])
      (vector-set! ys i (vector-ref xs i)))
    (cons (make-array xs) (make-array ys))))

;; pretty printing an array for debugging
(define (print-array xs)
  (for ([x (array-contents xs)])
    (printf "~a " (element-content x)))
  (newline))

;; Accessor to array without affecting R/W sets
(define (array-nth xs i)
  (element-content (vector-ref (array-contents xs) i)))

;; execute reduce with an array [1, 1, ..., 1]
(define (run-reduce red ntrd size)
  (define xs (create-array ntrd))
  (printf "before:\n")
  (print-array xs)
  (invoke-kernel red ntrd xs size (n-iter ntrd))
  (printf "after:\n")
  (print-array xs)
  xs)

;; verify that the two reduction algorithm are equivalent
;; Two reduction algorithm compute completely equivalent results
(define (verify-reduce ntrd size)
  (define xsys (create-sym-arrays ntrd))
  (define xs (car xsys))
  (define ys (cdr xsys))
  (verify
   #:guarantee 
   (begin
     (invoke-kernel reduce ntrd xs size (n-iter ntrd))
     (invoke-kernel reduce-opt ntrd ys size (n-iter ntrd))
     (printf "xs[0] = ~a\n" (array-nth xs 0))
     (printf "ys[0] = ~a\n" (array-nth ys 0))
     (printf "(eq? xs[0] ys[0]) = ~a\n" (eq? (array-nth xs 0) (array-nth ys 0)))
     (assert (eq? (array-nth xs 0) (array-nth ys 0))))))

(define (synth-reduce ntrd size)
  (define xs (for/vector ([i (in-range ntrd)])
               (define-symbolic* n integer?)
               n))
  (define arr (make-array (vector-map make-element xs)))
  (define sum (apply + (take (vector->list xs) size)))
  (synthesize
   #:forall (vector->list xs)
   #:guarantee 
   (begin
     (invoke-kernel reduce-opt-sketch ntrd arr size (n-iter ntrd))
     (printf "arr[0] = ~a\n" (array-nth arr 0))
     (assert (eq? (array-nth arr 0) sum)))))
