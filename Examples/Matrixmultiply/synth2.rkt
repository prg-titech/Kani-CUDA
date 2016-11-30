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

(define (mmul-kernel A B C n m l)
  (:= int BLOCK_SIZE (block-dim 0))
  (:= int row (+/LS (*/LS (block-idx 1) BLOCK_SIZE) (thread-idx 1)))
  (:= int col (+/LS (*/LS (block-idx 0) BLOCK_SIZE) (thread-idx 0)))
  
  (if- (&&/LS (</LS row (choose n l)) (</LS col (choose n l)))
       (begin
         (:= int brow (block-idx 1))
         (:= int bcol (block-idx 0))
         
         (:= int csub (+/LS (*/LS (*/LS l BLOCK_SIZE) (choose bcol brow)) (*/LS BLOCK_SIZE bcol)))
         
         (:= int trow (thread-idx 1))
         (:= int tcol (thread-idx 0))
         
         (:= int x 0)
         (:= int i 0)
         (for- [: (</LS i (quotient/LS (+/LS m (-/LS BLOCK_SIZE 1)) BLOCK_SIZE)) : (++ i)]
               (:= int asub (+/LS (*/LS (*/LS m BLOCK_SIZE) (choose bcol brow)) (*/LS BLOCK_SIZE i)))
               (:= int bsub (+/LS (*/LS (*/LS l BLOCK_SIZE) i) (*/LS BLOCK_SIZE (choose brow bcol))))
               
               (:shared int smemA[BLOCK_SIZE][BLOCK_SIZE])
               (:shared int smemB[BLOCK_SIZE][BLOCK_SIZE])

               (= [smemA trow tcol] [A (+/LS asub (+/LS tcol (*/LS m trow)))])
               (= [smemB trow tcol] [B (+/LS bsub (+/LS tcol (*/LS l trow)))])
               
               (choose (barrier) (void))

               (:= int k 0) 
               (for- [: (</LS k BLOCK_SIZE) : (++ k)]
                     (+= x (*/LS [smemA (choose tcol trow k) (choose tcol trow k)] [smemB (choose tcol trow k) (choose tcol trow k)])))
               
               (choose (barrier) (void)))
         (= [C (+/LS csub (+/LS (choose trow tcol) (*/LS (choose tcol trow) (choose n l))))] x))))




(define src1 (make-array (for/vector ([i (in-range 36)]) (make-element i)) 36))
(define src2 (make-array (for/vector ([i (in-range 36)]) (make-element i)) 36))
(define dst (make-array (for/vector ([i (in-range 36)]) (make-element i)) 36))

(define (array-eq-verify arr1 arr2 len)
  (for ([i (in-range len)])
    (assert
     (eq?
      (array-ref-host arr1 i)
      (array-ref-host arr2 i)))))

;(printmatrix dst 8 4)
;(invoke-kernel matrix-test 4 8 src1 src2 dst 8)
;(printmatrix src1 8 4)
;(invoke-kernel reduce 4 4 src1 8 8)

;(mmul-host src1 src2 4 4 4)

(print-matrix (mmul-host src1 src2 6 6 6) 6 6)

(define (kernel-out src1 src2 dst n)
  (begin
    (invoke-kernel
     mmul-kernel
     (list (quotient n 2) (quotient n 2)) '(2 2) src1 src2 dst n n n)
    dst))

;(kernel-out src1 src2 dst 6)

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