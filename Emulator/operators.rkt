#lang rosette

(require "work.rkt"
         rosette/lib/synthax)

(provide +/LS -/LS */LS //LS
         eq?/LS　!/LS &&/LS ||/LS </LS >/LS
         sin/LS cos/LS neq?/LS ==/LS
         quotient/LS modulo/LS %/LS
         ?:/LS min/LS max/LS &/LS)

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

;; Return a transposed matrix.
;; The type of argument is list of list, list of vector
(define (transpose xs)
  (match xs
    ['() '()]
    [(cons x '()) (for/all ([x x]) (for/list ([elem x]) (list elem)))]
    [(cons x rst)
     (for/all ([x x])
       (for/list ([head x]
                  [tail (transpose rst)])
         (cons head tail)))]))

;(define l
;    (for/list ([i 5])
;      (for/vector ([j 5])
;        (- i j))))

;(transpose l)

;; Lifting an operator on scalar values to an operator on vector
(define (LSop1 op)
  (lambda (x)
    (let ([x (vecfy x)])
      (map-vec op x))))

(define (LSop2 op)
  (lambda (x y)
    (let ([x (vecfy x)]
          [y (vecfy y)])
      (zipWith-vec op x y))))

(define (LSop-many-trans op)
  (lambda (x . xs)
    (let* ([xs (map vecfy (cons x xs))]
           [ys (transpose xs)])
      (for/vector ([y ys])
        (if (member 'masked-value y)
            'masked-value 
            (apply op y))))))

(define (LSop-many-rec op)
  (lambda (x xs)
    (if (null? xs)
        (vecfy x)
        ;        (begin (printf "~a\n" xs)
        ((LSop-many-rec op) ((LSop2 op) x (car xs)) (cdr xs)))))

(define (LSop-many op)
  (lambda (x . xs)
    ((LSop-many-rec op) x xs)))

;; Denotation of (b)? then-val : else-val
(define (?:/LS b then-cl else-cl)
    (let* ([bval (b)]
           [mthen (&&/LS bval (mask))]
           [melse (&&/LS (!/LS bval) (mask))]
           [then-val (parameterize ([mask mthen]) (then-cl))]
           [else-val (parameterize ([mask melse]) (else-cl))])
      (for*/all ([mthen mthen]
                 [melse melse]
                 [then-val then-val]
                 [else-val else-val])
        (for/vector ([i (block-size)])
          (cond [(and (eq? (vector-ref mthen i) #f)
                      (eq? (vector-ref melse i) #f))
                 'masked-value]
                [(eq? (vector-ref mthen i) #f)
                 (vector-ref (vecfy else-val) i)]
                [(eq? (vector-ref melse i) #f)
                 (vector-ref (vecfy then-val) i)]
                [else (begin
                        ;(print (vector-ref melse i))
                        ;(newline)
                        ;(printf "error: Check operators.rkt\n")
                        (assert false))])))))


;; Lifting basic operators
(define +/LS (LSop-many +))
(define -/LS (LSop-many -))
(define */LS (LSop-many *))
;(define //LS (LSop-many /))
(define (//LS a b) ((LSop1 exact->inexact) ((LSop2 /) a b)))
(define eq?/LS (LSop2 eq?))
(define ==/LS (LSop2 eq?))
(define !/LS (LSop1 !))
(define sin/LS (LSop1 sin))
(define cos/LS (LSop1 cos))
(define &&/LS (LSop-many &&))
(define ||/LS (LSop-many ||))
(define >/LS (LSop2 >))
(define </LS (LSop2 <))
(define quotient/LS (LSop2 quotient))
(define %/LS (LSop2 quotient))
(define modulo/LS (LSop2 modulo))
(define min/LS (LSop2 min))
(define max/LS (LSop2 max))
(define (neq?/LS a b) (!/LS (eq?/LS a b)))
(define (&/LS x) x)

(define b (choose (vecfy 1) (vecfy 2)))
(define a (choose 1 2))
(define c (vecfy a))