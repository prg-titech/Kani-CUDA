#lang rosette

(require "../lang.rkt")

(define arr (make-array (for/vector ([i (in-range 10)]) (make-element 0))))

(define (test arr)
  (:= int x [arr 0])
  (if- (eq?/LS (tid) (- (thread-dim) 1))
       (= [arr 0] 1)))

(define (test-block-race arr)
  (: int x) 
  (if (eq? (bid) 0)
       (begin
         (if- (eq?/LS (tid) 0)
              (= [arr 0] 4))
         (barrier)
         (if- (eq?/LS (tid) 1)
              (= x [arr 0])))
       (= x [arr 0])))

(invoke-kernel test-block-race 2 5 arr)

(for ([i (in-range 10)]) (print (element-content (vector-ref (array-contents arr) i))))