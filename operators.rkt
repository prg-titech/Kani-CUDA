#lang rosette

(require "work.rkt"
         rosette/lib/synthax)

(provide +/LS -/LS */LS //LS eq?/LS !/LS &&/LS </LS >/LS quotient/LS modulo/LS)

;; map, zipWith
;; 'masked-value は mask されたスレッドが返す値を表し,map, zipWith は 'masked-value を無視する
(define (zipWith-vec-const f xs ys)
  (for/vector ([x xs]
               [y ys])
    (if (or (eq? x 'masked-value) (eq? y 'masked-value)) 'masked-value
        (f x y))))

(define (zipWith-vec f xs ys)
    (for*/all ([f f]
             [xs xs]
             [ys ys])
      (zipWith-vec-const f xs ys)))
      

(define (map-vec f xs)
  (for/vector ([x xs])
    (if (eq? x 'masked-value) 'masked-value
        (f x))))

;; lifting an operator on scalar values to an operator on vector
(define (LSop op)
  (lambda (x)
    (let ([x (vecfy x)])
      (map-vec op x))))

(define (LSop2 op)
  (lambda (x y)
    (let ([x (vecfy x)]
          [y (vecfy y)])
      (zipWith-vec op x y))))

;; lifting basic operators
(define +/LS (LSop2 +))
(define -/LS (LSop2 -))
(define */LS (LSop2 *))
(define //LS (LSop2 /))
(define eq?/LS (LSop2 eq?))
(define !/LS (LSop !))
(define &&/LS (LSop2 &&))
(define >/LS (LSop2 >))
(define </LS (LSop2 <))
(define quotient/LS (LSop2 quotient))
(define modulo/LS (LSop2 modulo))

(define x (for/vector ([i (in-range 10)]) i))
(define y (for/vector ([i (in-range 10)]) i))
(define xory (choose x y))