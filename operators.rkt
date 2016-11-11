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


(define (map-vec-const f xs)
  (for/vector ([x xs])
    (if (eq? x 'masked-value) 'masked-value
        (f x))))

(define (map-vec f xs)
  (for*/all ([f f]
             [xs xs])
    (map-vec-const f xs)))

;; lifting an operator on scalar values to an operator on vector
(define (LSop1 op)
  (lambda (x)
    (let ([x (vecfy x)])
      (map-vec op x))))

(define (LSop2 op)
  (lambda (x y)
    (let ([x (vecfy x)]
          [y (vecfy y)])
      (zipWith-vec op x y))))

;; Return a transposed matrix.
;; The type of argument is list of list, list of vector
(define (transpose xs)
  (match xs
    ['() '()]
    [(cons x '()) (for/list ([elem x]) (list elem))]
    [(cons x rst)
     (for/list ([head x]
                [tail (transpose rst)])
       (cons head tail))]))

;(define l
;    (for/list ([i 5])
;      (for/vector ([j 5])
;        (- i j))))

;(transpose l)

(define (LSop-many op)
  (lambda (x . xs)
    (let* ([xs (map vecfy (cons x xs))]
           [ys (transpose xs)])
      (for/vector ([y ys])
        (if (member 'masked-value y)
            'masked-value
            (apply op y))))))

;; lifting basic operators
(define +/LS (LSop-many +))
(define -/LS (LSop-many -))
(define */LS (LSop-many *))
(define //LS (LSop-many /))
(define eq?/LS (LSop2 eq?))
(define !/LS (LSop1 !))
(define &&/LS (LSop-many &&))
(define >/LS (LSop2 >))
(define </LS (LSop2 <))
(define quotient/LS (LSop2 quotient))
(define modulo/LS (LSop2 modulo))