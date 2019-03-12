#lang rosette

(require rosette/lib/synthax)

(provide  grid-dimension grid-dim grid-size block-dimension block-dim
          bid bid-3d block-idx block-size mask thread-idx vecfy to-bid-3d
          tid barrier-count mask incl-bc clear-bc get-bc
          env clear-env add-env profiled-vars set-profiled-vars!
          profile-vars puts cudaDeviceSynchronize)

(define barrier-count (make-parameter 0))

;; Model of grid structure
(define grid-dimension (make-parameter '(1)))

(define (grid-dim idx) (list-ref (grid-dimension) idx))

(define (grid-size) (apply * (grid-dimension)))


;; Model of block stracture
(define bid (make-parameter 0))

(define bid-3d (make-parameter '(0)))

(define (block-idx idx) (list-ref (bid-3d) idx))

(define block-dimension (make-parameter '(16)))

(define (block-dim idx) (list-ref (block-dimension) idx))

(define (block-size) (apply * (block-dimension)))

;; Convert a block number(integer) to block ID(list of int)
(define (to-bid-3d bid)
  (cond
    [(eq? (length (grid-dimension)) 1) (list bid 1 1)]
    [(eq? (length (grid-dimension)) 2) (list (modulo bid (grid-dim 0)) (quotient bid (grid-dim 0)) 1)]
    [else (list (modulo (modulo bid (* (grid-dim 0) (grid-dim 1))) (grid-dim 0))
                (quotient (modulo bid (* (grid-dim 0) (grid-dim 1))) (grid-dim 0))
                (quotient bid (* (grid-dim 0) (grid-dim 1))))]))


;; Model of threads
(define (tid) (for/vector ([i (block-size)]) i))

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


;; 'mask' is used to prevent some threads from running
(define mask (make-parameter (make-vector (block-size) #t)))


;; Convert a scalar value to a vector value
(define (vecfy x)
  (for/all ([x x])
    ;(printf "vecfy x = ~a\n" x)
    (cond [(or (integer? x) (boolean? x) (real? x)) (vector->immutable-vector (make-vector (block-size) x))]
          [(vector? x) x]
          [else (raise "vecfy: expected an integer/boolean or a vector")])))


;==========================
(define barrier-counter 0)

(define (get-bc)
  barrier-counter)

(define (incl-bc)
  (set! barrier-counter (+ barrier-counter 1)))

(define (clear-bc)
  (set! barrier-counter 0))

(define env
  (make-hash))

(define (clear-env)
  (set! env (make-hash)))

(define (add-env x v)
  (hash-set! env x v))

(define profiled-vars
  "")

(define (set-profiled-vars! str)
  (set! profiled-vars str))

(define (profile-vars str)
  (define out (open-output-file "profiles/vars" #:exists 'truncate))
  (fprintf out (string-append "tid bid id smid " str))
  (close-output-port out))

(define (puts x)
  (println x)
  x)

(define (cudaDeviceSynchronize)
  0)








