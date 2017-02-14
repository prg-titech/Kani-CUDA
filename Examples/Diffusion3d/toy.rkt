#lang rosette

(require "diffusion3d-baseline.rkt"
         "../../lang.rkt")

(current-bitwidth 10)

(define (r)
  (define-symbolic* r real?)
  r)

(define in (make-array (for/vector ([i 2]) (make-element (r))) 2))
(define out (make-array (for/vector ([i 2]) (make-element (array-ref-host in i))) 2))
;(define out2 (make-array (for/vector ([i 2]) (make-element (array-ref-host in i))) 2))

(define (test-array-spec in out)
  (:= int zero 0)
  (:= int one 1)
  (:= int i (?: (eq?/LS (thread-idx 0) 1) zero one))
  (= [out i] (*/LS (+/LS (thread-idx 0) 10) [in i])))

(define (test-array in out)
  (:= int zero 0)
  (:= int one 1)
  (if- (eq?/LS 0 (choose 0 1))
       (begin 
         (:= int i (?: (eq?/LS (thread-idx 0) (choose zero one)) (choose zero one) (choose zero one)))
         (printf "~a\n" i)
         (= [out i] (*/LS (+/LS (thread-idx 0) 10) [in i])))))

(define (array-eq-verify arr1 arr2 len)
  (for ([i (in-range len)])
    (assert
     (eq?
      (array-ref-host arr1 i)
      (array-ref-host arr2 i)))))


(define (synth-stencil)
  (time
   (synthesize #:forall (list in)
               #:guarantee (begin
                             (invoke-kernel test-array-spec '(1) '(2) in out)
                             (invoke-kernel test-array '(1) '(2) in out)
                             (array-eq-verify
                              out out 2)))))

(define (rotate arr)
  (:= int i (thread-idx 0))
  (:= int x [arr i])
  (barrier)
  (= [arr (modulo/LS (+/LS i 1) (block-dim 0))] x))

(define (rotates arr SIZE)
  (:= int i (thread-idx 0))
  (:= int x [arr i])
  (choose (barrier) (void))
  (= [arr (modulo/LS (+/LS i (??)) SIZE)] x))

(for/list ([SIZE (in-range 10 1001 2)])
  ;(define SIZE 20)
  (define in0 (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
  (define in1 (make-array (for/vector ([i 5]) (make-element i)) 5))
  (define in2 (make-array (for/vector ([i 4]) (make-element i)) 4))
  (define out0 (make-array (for/vector ([i SIZE]) (make-element (modulo (+ i -1) SIZE))) SIZE))
  (define out2 (make-array (for/vector ([i 5]) (make-element (modulo (+ i -1) 4))) 4))
  
  (define (spec-rotate)
    (begin 
      (invoke-kernel rotates '(1) (list SIZE) in0 SIZE)
      (invoke-kernel rotates '(1) '(4) in2 4)
      (for ([i SIZE]) (assert (eq? (array-ref-host in0 i) (array-ref-host out0 i))))
      (for ([i 4]) (assert (eq? (array-ref-host in2 i) (array-ref-host out2 i))))))
  
  
  (define (synth-rotate)
    (define-values (_ cpu real gc)
      (time-apply
       (lambda () (synthesize #:forall '()
                              #:guarantee (spec-rotate)))
       '()))
    (list cpu real gc))

  (printf "~a ~a" (car (synth-rotate)) (cadr (synth-rotate)))
  (newline)
  )
  ;(print-matrix out0 5 1)
  
  ;(map syntax->datum (generate-forms (synth-rotate))))

;(define (rotate0 arr) (:= int i (thread-idx 0)) (:= int x (arr i)) (barrier) (= (arr (modulo/LS (+/LS i 8) (block-dim 0))) x))

;(invoke-kernel rotate0 '(1) '(5) in1)
;(print-matrix in1 5 1)

