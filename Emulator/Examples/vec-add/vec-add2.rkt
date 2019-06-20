#lang rosette

(require "lang.rkt")

;; vector addition from Sec. 1.1 of ``A Hoare Logic for SIMT Programs''
(define (vec-add out arr1 arr2 len)
  (:= int ix (+/LS (thread-idx 0) (*/LS (block-idx 0) (block-size))))
  (while (</LS ix len) #:bound len
;         (printf "ix = ~a\n" ix)
         (= [out ix] ((choose -/LS +/LS) [arr1 ix] [arr2 ix]))
         (= ix (+/LS ix (choose (*/LS 2 (block-size)) (block-size)))))
  )

;(define out (new-sh-array 100 integer?))
(define out (make-array (for/vector ([i (in-range 10)]) (make-element 0)) 10))
(define in1 (make-array (for/vector ([i (in-range 10)]) (make-element i)) 10))
(define in2 (make-array (for/vector ([i (in-range 10)]) (make-element i)) 10))
(define ans (make-array (for/vector ([i (in-range 10)]) (make-element (* 2 i))) 10))
(define (array-eq-verify arr1 arr2 len)
  (define cont1 (array-contents arr1))
  (define cont2 (array-contents arr2))
  (for ([i (in-range len)])
    (assert
     (eq?
      (element-content (vector-ref cont1 i))
      (element-content (vector-ref cont2 i))))))

(define out1
  (begin
    (invoke-kernel vec-add '(2) '(5) out in1 in2 10)
    out))

(define p 
 (synthesize
  #:forall '()
  #:guarantee (array-eq-verify out1 ans 10)))
