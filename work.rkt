#lang rosette

(provide ntid tid mask vecfy)

;; number of threads and mask
;; before running the kernel, ntid need to be specified with a number: 
;;   (parameterize ([ntid ?]) (kernel))
(define ntid (make-parameter 16))
;; mask is only internally used  
(define mask (make-parameter (make-vector (ntid) #t)))

;; thread id
(define (tid) (for/vector ([i (in-range (ntid))]) i))

;; convert a scalar value to a vector value
(define (vecfy x)
  ;(printf "vecfy x = ~a\n" x)
  (cond [(or (integer? x) (boolean? x)) (make-vector (ntid) x)]
        [(vector? x) x]
        [else (raise "vecfy: expected an integer/boolean or a vector")]))