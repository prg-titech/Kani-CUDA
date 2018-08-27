#lang rosette

(require "../../lang.rkt")

(define arrA (make-array (for/vector ([i 10]) (make-element i)) 10))
(define arrB (make-array (for/vector ([i 10]) (make-element 0)) 10))

(:= int barrier-count 0)

(define (array-copy A B)
  (:= int i (thread-idx 0))
  (:= int x [A i])
  
  (choose (void) (begin (barrier) (++ barrier-count)))
  (choose (void) (begin (barrier) (++ barrier-count)))
  (choose (void) (begin (barrier) (++ barrier-count)))
  (choose (void) (begin (barrier) (++ barrier-count)))
  
  (= [B i] x))

(define (array-eq-verify arr1 arr2 len)
  (for ([i (in-range len)])
    (assert
     (eq?
      (array-ref-host arr1 i)
      (array-ref-host arr2 i)))))

(define ans (invoke-kernel array-copy '(1) '(10) arrA arrB))

(define (synth)
  (map syntax->datum
       (generate-forms
        (time
         (optimize #:maximize (list (vector-ref barrier-count 0))
                   #:guarantee (array-eq-verify arrA arrB 10))))))

(synth)



