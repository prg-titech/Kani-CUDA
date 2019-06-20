#lang rosette

(require "lang.rkt")

(define (reduce arr len)
  (:= int s (quotient/LS len 2))
  (while (>/LS s 0)
         (if- (</LS (tid) s)
              (= [arr (tid)] (+/LS [arr (tid)] [arr (+/LS (tid) s)])))
         (= s (quotient/LS s 2))
         (barrier)))

(define xs (make-array (for/vector ([i (in-range 32)]) (make-element 1))))
(invoke-kernel reduce 1 16 xs 32)

(for ([i (in-range 32)])
  (printf "~a " (element-content (vector-ref (array-contents xs) i))))
(newline)