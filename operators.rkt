#lang rosette

(require "work.rkt"
         rosette/lib/synthax)

(provide +/LS -/LS */LS //LS
         eq?/LS　!/LS &&/LS </LS >/LS
         quotient/LS modulo/LS
         ?:/LS)

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
     (for*/all ([x x]
               [rst rst])
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
        (begin (printf "~a\n" xs)
               ((LSop-many-rec op) ((LSop2 op) x (car xs)) (cdr xs))))))

(define (LSop-many op)
  (lambda (x . xs)
    ((LSop-many-rec op) x xs)))

;; Denotation of (b)? then-val : else-val
(define (?:/LS b then-cl else-cl)
  (let ([bval (b)])
    (let ([then-val (parameterize ([mask (&&/LS bval (mask))]) (then-cl))]
          [else-val (parameterize ([mask (&&/LS (!/LS bval) (mask))]) (else-cl))]
          [mthen (&&/LS bval (mask))]
          [melse (&&/LS (!/LS bval) (mask))])
      (for/vector ([i (block-size)])
        (cond [(and (eq? (vector-ref mthen i) #f)
                    (eq? (vector-ref melse i) #f))
               'masked-value]
              [(eq? (vector-ref mthen i) #f)
               (vector-ref (vecfy else-val) i)]
              [(eq? (vector-ref melse i) #f)
               (vector-ref (vecfy then-val) i)]
              [else (begin (printf "check error\n")
                           (assert false))])))))


;; Lifting basic operators
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

(define b (choose (vecfy 1) (vecfy 2)))
(define a (choose 1 2))
(define c (vecfy a))