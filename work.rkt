#lang rosette

(require rosette/lib/synthax)

(provide  grid-dimension grid-dim grid-size block-dimension block-dim
          block-index block-idx block-size mask thread-idx vecfy to-bid
          tid bid barrier-count mask incl-bc clear-bc get-bc out-file)

(define barrier-count (make-parameter 0))

;; Model of grid structure
(define grid-dimension (make-parameter '(1)))

(define (grid-dim idx) (list-ref (grid-dimension) idx))

;; Return current grid size
(define (grid-size) (apply * (grid-dimension)))

;; Return current block id
(define block-index (make-parameter '(0)))

(define (block-idx idx) (list-ref (block-index) idx))

;; Model of block stracture
(define block-dimension (make-parameter '(16)))

(define (block-dim idx) (list-ref (block-dimension) idx))

;; Return current block size 
(define (block-size) (apply * (block-dimension)))

;; mask is only internally used  
(define mask (make-parameter (make-vector (block-size) #t)))

;; Convert a scalar value to a vector value
(define (vecfy x)
  (for/all ([x x])
    ;(printf "vecfy x = ~a\n" x)
    (cond [(or (integer? x) (boolean? x) (real? x)) (vector->immutable-vector (make-vector (block-size) x))]
          [(vector? x) x]
          [else (begin (print x)
                       (newline)
                       (raise "vecfy: expected an integer/boolean or a vector"))])))

;; Convert a block number(integer) to block ID(list of int)
(define (to-bid i)
  (cond
    [(eq? (length (grid-dimension)) 1) (list i)]
    [(eq? (length (grid-dimension)) 2) (list (modulo i (grid-dim 0)) (quotient i (grid-dim 0)))]
    [else (list (modulo (modulo i (* (grid-dim 0) (grid-dim 1))) (grid-dim 0))
                (quotient (modulo i (* (grid-dim 0) (grid-dim 1))) (grid-dim 0))
                (quotient i (* (grid-dim 0) (grid-dim 1))))]))

;; Return current thread ID(vector of int), idx represent a dimension thread ID
;; Use in kernel
(define (thread-idx idx)
  (let ([bdim (length (block-dimension))])
    (if (< idx bdim)
        (cond
          [(eq? bdim 1) (for/vector ([i (block-size)]) i)]
          [(eq? bdim 2) (if (eq? idx 0)
                            (for/vector ([i (block-size)]) (modulo i (block-dim 0)))
                            (for/vector ([i (block-size)]) (quotient i (block-dim 0))))]
          [(eq? bdim 3) (cond
                          [(eq? idx 0) (for/vector ([i (block-size)])
                                         (modulo (modulo i (* (block-dim 0) (block-dim 1))) (block-dim 0)))]
                          [(eq? idx 1) (for/vector ([i (block-size)])
                                         (quotient (modulo i (* (block-dim 0) (block-dim 1))) (block-dim 0)))]
                          [(eq? idx 2) (for/vector ([i (block-size)])
                                         (quotient i (* (block-dim 0) (block-dim 1))) (block-dim 0))])])
        (assert false))))


(define (tid) (for/vector ([i (block-size)]) i))

(define bid (make-parameter 0))

(define out-file (make-parameter 0))

(define barrier-counter 0)

(define (get-bc)
  barrier-counter)

(define (incl-bc)
  (set! barrier-counter (+ barrier-counter 1)))

(define (clear-bc)
  (set! barrier-counter 0))










