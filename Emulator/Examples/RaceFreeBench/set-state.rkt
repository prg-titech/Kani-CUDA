#lang rosette

;; Preliminary benchmark of list set vs. 3-state

;; Set represented as list
(define (list-set-mem? l x) (pair? (member x l)))

(define (list-set-memf? l x) (pair? (memf x l)))

(define (list-set-add l x)
  (if (list-set-mem? l x) l (cons x l)))
  
(define (list-set-race-free? l x)
  (not (list-set-memf? l (lambda (y) (not (eqv? x y))))))

(define list-set-empty '())

;; Set represented as 3-state
;; -1 <-> {}, n <-> {n}, 'Many <-> {n1, n2, ...}
(define (kani-set-add s x)
  (cond 
   ([and (number? s) (< s 0)] x)
   ([and (number? s) (eqv? s x)] s)
   ([and (number? s) (not (eqv? s x))] 'Many)
   ([(not (number? s))] 'Many)))

(define (kani-race-free? l x)
  (and (number? l) (or (< l 0) (eqv? l x))))

(define kani-set-empty -1) 

;; number of threads, length of the accessed array, number of iterations
(define n-th 10)
(define len-arr 20)
<<<<<<< Updated upstream
(define n-iter 30)
=======
(define n-iter 10)
>>>>>>> Stashed changes

(define (write-arr v tid add race-free?)
  (define-symbolic* idx integer?)
  (let ([tset (vector-ref v idx)])
    (printf "write to ~a\n" tid)
    (assert (race-free? tset tid))
    (vector-set! v idx (add tset tid))))

(define (add-many v n add race-free?)
  (for ([_ (in-range n)])
    (write-arr v (random n-th) add race-free?)))

(define (test-set add race-free? emp init)
  (define v (make-vector len-arr emp))
  (random-seed init)
  (add-many v n-iter add race-free?)
  v)

(define (kani-test-set init) (test-set kani-set-add kani-race-free? kani-set-empty init))
(define (list-test-set init) (test-set list-set-add list-set-race-free? list-set-empty init))