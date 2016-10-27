#lang rosette

(require "../../lang.rkt")

;; Multiplies two squre matrices A and B, where the dimension of A is 
;; n x p and dimension of B is p x m. 
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
  (:= int block-x ((choose modulo/LS quotient/LS) (bid) gsize-x))
  (:= int block-y (quotient/LS (bid) gsize-x))
  ;(printf "~a\n" block-x)
  ;(printf "~a\n" block-y)
  (:= int thread-x (modulo/LS (tid) 2))
  (:= int thread-y (quotient/LS (tid) 2))
  (choose (barrier) (void))
  (if- (eq?/LS (tid) 0)
       (for ([i (in-range (* 2 p))])
         (= [smemA i]
            [A (+/LS i (*/LS (choose block-x block-y) (* 2 p)))])
         (= [smemB i]
            [B (+/LS
                (+/LS
                 (*/LS (modulo/LS i p) m)
                 (*/LS (choose block-x block-y) 2)) (quotient/LS i p))])))
  (print [A (choose block-x block-y)])
  (newline)
  (print (choose block-x block-y))
  (newline)
  (choose (barrier) (void))
  (:= int x 0)
  (for ([i (in-range p)])
    (+= x (*/LS [smemA (+/LS i (*/LS (choose thread-x thread-y) p))]
                [smemB (+/LS i (*/LS (choose thread-x thread-y) p))])))
  (choose (barrier) (void))
  (= [C (+/LS
         (+/LS
          (+/LS
           (*/LS
            (choose block-x block-y)
            (*/LS 2 p))
           (*/LS 2 (choose block-x block-y)))
          (*/LS (choose thread-x thread-y) p))
         (choose thread-x thread-y))]
     x))


(define src1 (make-array (for/vector ([i (in-range 36)]) (make-element i))))
(define src2 (make-array (for/vector ([i (in-range 36)]) (make-element i))))
(define dst (make-array (for/vector ([i (in-range 36)]) (make-element i))))

(define (array-eq-verify arr1 arr2 len)
  (define cont1 (array-contents arr1))
  (define cont2 (array-contents arr2))
  (for ([i (in-range len)])
    (assert
     (eq?
      (element-content (vector-ref cont1 i))
      (element-content (vector-ref cont2 i))))))

;(printmatrix dst 8 4)
;(invoke-kernel matrix-test 4 8 src1 src2 dst 8)
;(printmatrix src1 8 4)
;(invoke-kernel reduce 4 4 src1 8 8)

;(mmul-host src1 src2 4 4 4)

(printmatrix (mmul-host src1 src2 6 6 6) 6 6)

(define (kernel-out src1 src2 dst n)
  (begin
    (invoke-kernel mmul-kernel (* (quotient n 2) (quotient n 2)) 4 src1 src2 dst n n n)
    dst))

;(printmatrix out1 4 4)

;(array-eq-verify (mmul-host src1 src2 4 4 4) out1 16)

(define (synth-vector n)
  (define (i)
    (define-symbolic* i integer?)
    i)
  (define A (make-array (for/vector ([j (in-range (* n n))]) (make-element (i))) (* n n)))
  (define B (make-array (for/vector ([j (in-range (* n n))]) (make-element (i))) (* n n)))
  (define C (make-array (for/vector ([j (in-range (* n n))]) (make-element 0)) (* n n)))
  (synthesize
   #:forall (append (list A) (list B))
   #:guarantee (array-eq-verify (mmul-host A B n n n) (kernel-out A B C n) (* n n))))

(map syntax->datum (generate-forms (synth-vector 4)))