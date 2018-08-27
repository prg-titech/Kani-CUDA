#lang rosette

(require "diffusion3d-baseline.rkt"
         "../../lang.rkt"
         (rename-in rosette/lib/synthax [choose ?])
         racket/hash)

(current-bitwidth #f)

(define switch1 #f)
(define switch2 #f)
(define switch3 #f)

;; 0  1  2     0  1  3
;; 3  4  5 =>  6  7  9  
;; 6  7  8    12 13 15
;; Input array is a flat array.
(define (test arr)
  (: int i)
  
  ; Get thread ID
  (when switch1 (= i (thread-idx (? 0 1))))
  (when (not switch1) (= i (thread-idx 0)))
  
  ; Desired code
  ; int a = (i % 3 == 0) ? i : (i - 1);
  
  ; Expression with hole
  ; (? (i % 3) 0 1 2 i)
  ;(:= int a (stencil-a-exp (modulo/LS i 3) (make-vector 9 1) (make-vector 9 0) 2 i 0))
  ;(:= int a (?: (eq?/LS (modulo/LS i 3) (? 0 1)) i (-/LS i (? 0 1))))
  (: int a)
  (when switch2 (= a (?: (eq?/LS (modulo/LS i 3) (? 0 1)) i (-/LS i (? 0 1)))))
  (when (not switch2) (= a (?: (eq?/LS (modulo/LS i 3) 0) i (-/LS i 1))))
  
  (:= int b [arr a])
  (when switch3 (? (barrier) (void)))
  (when (not switch3) (barrier))
  
  (= [arr i] (+/LS [arr i] b)))


;; Add a constraint that it is equal to each of the elements of
;; two arrays, arr1 and arr2, to asserts.
(define (array-eq-verify arr1 arr2 len)
  (for ([i (in-range len)])
    (assert
     (eq?
      (array-ref-host arr1 i)
      (array-ref-host arr2 i)))))


(define (synth-test)
  (time
   (synthesize #:forall '()
               #:guarantee (begin
                             (define SIZE 9)
                             (define in (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
                             (define out (make-array (vector (make-element 0)
                                                             (make-element 1)
                                                             (make-element 3)
                                                             (make-element 6)
                                                             (make-element 7)
                                                             (make-element 9)
                                                             (make-element 12)
                                                             (make-element 13)
                                                             (make-element 15)) SIZE))
                             (invoke-kernel test '(1) '(9) in)
                             (array-eq-verify in out SIZE)
                             ))))

;(map syntax->datum (generate-forms (synth-test)))

(set! switch1 #t)
(define ans (model (synth-test)))
(set! switch1 #f)
(set! switch2 #t)
(set! ans (hash-union ans (model (synth-test))))
(set! switch2 #f)
(set! switch3 #t)
(set! ans (hash-union ans (model (synth-test)))) 

;(define test-hash (model (synth-test)))

(map syntax->datum (generate-forms (sat ans)))
