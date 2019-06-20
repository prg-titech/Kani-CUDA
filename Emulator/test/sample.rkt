#lang rosette

(require "../lang.rkt")

;(define (test j)
;(printf "~a\n" (thread-idx i))
;(printf "~a\n" (block-idx i))
;(for- i 0 5 1
;      (print i)))

;(invoke-kernel test '(2 5) '(2 3) 0)

;(invoke-kernel test '(2 5) '(2 3) 1)

(define (same i)
  (and (eq? i (??)) (eq? (??) (??))))

(define (poly x)
  (+ (* x x x x) (* 6 x x x) (* 11 x x) (* 6 x)))

(define (a x)
  (* (+ x 1) (+ x (??)) (+ x (??)) (+ x (??))))

;(current-bitwidth #f)

(define-symbolic i i2 integer?)

(synthesize #:forall '()
            #:guarantee (assert (eq? i i)))

