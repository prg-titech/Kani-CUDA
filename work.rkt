#lang rosette

(provide thread-dim tid bid mask vecfy block-dim block-size)

;; TODO ntid -> thread-dim
;; number of threads and mask
;; before running the kernel, ntid need to be specified with a number: 
;;   (parameterize ([ntid ?]) (kernel))
(define thread-dim (make-parameter 16))
;; mask is only internally used  
(define mask (make-parameter (make-vector (thread-dim) #t)))

;; Model of block stracture
(define block-dim (make-parameter 1))

;; Return current block size 
(define (block-size) (block-dim))

;; Return current thread id
(define (tid) (for/vector ([i (in-range (thread-dim))]) i))

;; Return current block id
(define bid (make-parameter 0))

;; convert a scalar value to a vector value
(define (vecfy x)
  ;(printf "vecfy x = ~a\n" x)
  (cond [(or (integer? x) (boolean? x)) (make-vector (thread-dim) x)]
        [(vector? x) x]
        [else (raise "vecfy: expected an integer/boolean or a vector")]))







