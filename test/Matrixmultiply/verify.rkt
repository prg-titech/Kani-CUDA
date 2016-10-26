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
  (:shared int smemA ((* 2 p)))
  (:shared int smemB ((* 2 p)))
  (:= int gsize-x (quotient/LS (choose n m) 2))
  (:= int gsize-y (quotient/LS (choose n m) 2))
  (:= int block-x (modulo/LS (bid) gsize-x))
  (:= int block-y (quotient/LS (bid) gsize-x))
  (printf "~a\n" block-x)
  (printf "~a\n" block-y)
  (:= int thread-x (modulo/LS (tid) 2))
  (:= int thread-y (quotient/LS (tid) 2))
  (if-
   (eq?/LS (tid) 0)
   (for
       ((i (in-range (* 2 p))))
     (= (smemA i) (A (+/LS i (*/LS block-x (* 2 p)))))
     (if-
      (</LS i p)
      (= (smemB i) (B (+/LS (*/LS i m) (*/LS block-y 2))))
      (= (smemB i) (B (+/LS (+/LS (*/LS (-/LS i p) m) (*/LS block-y 2)) 1))))))
  (print (choose block-x block-y))
  (newline)
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
    (invoke-kernel mmul-kernel (* (quotient n 2) (quotient n 2)) 4 src1 src2 dst n n n)
    dst))

(define (array-eq-verify arr1 arr2 len)
  (define cont1 (array-contents arr1))
  (define cont2 (array-contents arr2))
  (for ([i (in-range len)])
    (assert
     (eq?
      (element-content (vector-ref cont1 i))
      (element-content (vector-ref cont2 i))))))

(define (verify-vector n)
  (define (i)
    (define-symbolic* i integer?)
    i)
  (define A (make-array (for/vector ([j (in-range (* n n))]) (make-element (i)))))
  (define B (make-array (for/vector ([j (in-range (* n n))]) (make-element (i)))))
  (define C (make-array (for/vector ([j (in-range (* n n))]) (make-element 0))))
  (verify (array-eq-verify (mmul-host A B n n n) (kernel-out A B C n) (* n n))))