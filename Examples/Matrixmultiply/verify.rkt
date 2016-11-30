#lang rosette

(require "../../lang.rkt")

(define (mmul-host A B n p m)
  (define C (make-array (for/vector ([i (in-range (* n m))]) (make-element 0))))
  (for ([i (in-range n)])
    (for ([j (in-range m)])
      (for ([k (in-range p)])
        (let ([a (array-ref-host A (+ (* i p) k))]
              [b (array-ref-host B (+ (* k m) j))]
              [c (array-ref-host C (+ (* i m) j))])
          (array-set-host! C (+ (* i m) j) (+ c (* a b)))))))
  C)

(define (mmul-kernel A B C n p m)
  (:shared int smemA[(* 2 p)])
  (:shared int smemB[(* 2 p)])
  (:= int gsize-x (quotient/LS (choose n m) 2))
  (:= int gsize-y (quotient/LS (choose n m) 2))
  (:= int block-x (modulo/LS (block-idx 0) gsize-x))
  (:= int block-y (quotient/LS (block-idx 0) gsize-x))
  (:= int thread-x (modulo/LS (thread-idx 0) 2))
  (:= int thread-y (quotient/LS (thread-idx 0) 2))
  (if-
   (eq?/LS (thread-idx 0) 0)
   (for
       ((i (in-range (* 2 p))))
     (= (smemA i) (A (+/LS i (*/LS block-x (* 2 p)))))
     (if-
      (</LS i p)
      (= (smemB i) (B (+/LS (*/LS i m) (*/LS block-y 2))))
      (= (smemB i) (B (+/LS (+/LS (*/LS (-/LS i p) m) (*/LS block-y 2)) 1))))))
  (barrier)
  (:= int x 0)
  (for
      ((i (in-range p)))
    (+= x (*/LS (smemA (+/LS i (*/LS thread-x p))) (smemB (+/LS i (*/LS thread-y p))))))
  (=
   (C (+/LS (+/LS (+/LS (*/LS block-x (*/LS 2 p)) (*/LS 2 block-y)) (*/LS thread-x p)) thread-y))
   x))

(define (kernel-out src1 src2 dst n)
  (begin
    (invoke-kernel
     mmul-kernel
     (list (* (quotient n 2) (quotient n 2)))
     '(4)
     src1 src2 dst n n n)
    dst))


(define (array-eq-verify arr1 arr2 len)
  (for ([i (in-range len)])
    (assert
     (eq?
      (array-ref-host arr1 i)
      (array-ref-host arr2 i)))))

(define (verify-vector n)
  (define (i)
    (define-symbolic* i integer?)
    i)
  (define A (make-array (for/vector ([j (in-range (* n n))]) (make-element (i))) (* n n)))
  (define B (make-array (for/vector ([j (in-range (* n n))]) (make-element (i))) (* n n)))
  (define C (make-array (for/vector ([j (in-range (* n n))]) (make-element 0)) (* n n)))
  (verify (array-eq-verify (mmul-host A B n n n) (kernel-out A B C n) (* n n))))