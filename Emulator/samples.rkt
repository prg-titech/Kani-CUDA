#lang rosette

(require "while.rkt")

;; vector addition from Sec. 1.1 of ``A Hoare Logic for SIMT Programs''
(define (vec-add out arr1 arr2 len)
  (:= int ix (tid))
  (while (</LS ix len)
         ; (printf "ix = ~a\n" ix)
         (= [out ix] (+/LS [arr1 ix] [arr2 ix]))
         (= ix (+/LS ix (ntid))))
  )

;(define out (new-sh-array 100 integer?))
(define out (array (for/vector ([i (in-range 100)]) (make-element 0))))
(define in1 (array (for/vector ([i (in-range 100)]) (make-element i))))
(define in2 (array (for/vector ([i (in-range 100)]) (make-element i))))
(invoke-kernel vec-add 32 out in1 in2 100)
(for ([i (in-range 100)])
  (print (element-content (vector-ref (array-contents out) i)))
  (newline)
  )

;; parallel reduction from Sec. 3.3 of ``A Hoare Logic for SIMT Programs''
(define (reduce arr len)
  (:= int s (quotient/LS (scalar->vec len) 2))
  (while (>/LS s 0)
         (if- (</LS (tid) s)
              (= [arr (tid)] (+/LS [arr (tid)] [arr (+/LS (tid) s)])))
         (= s (quotient/LS s 2))
         (barrier)))

(define xs (array (for/vector ([i (in-range 32)]) (make-element i))))
(invoke-kernel reduce 16 xs 32)

;; a sample that causes barrier divergence
(define (test-barrier-divergence)
  (if- (</LS tid 8)
       (begin (barrier))))

;; (test-barrier-divergence)
;;
