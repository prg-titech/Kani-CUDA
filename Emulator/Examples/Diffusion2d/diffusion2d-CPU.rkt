#lang rosette

(require "../../lang.rkt"
         "diffusion2d-GPU.rkt"
         "diffusion2d-h.rkt"
         "diffusion2d-GPU-shared.rkt"
         (rename-in rosette/lib/synthax [choose ?])
         racket/hash)


(define (r)
  (define-symbolic* r real?)
  r)

(define SIZE (* Nx Ny)) 

(define u (make-array (for/vector ([i SIZE]) (make-element (r))) SIZE))
(define u-s (make-array (for/vector ([i SIZE]) (make-element (array-ref-host u i))) SIZE))
(define ulap (make-array (for/vector ([i SIZE]) (make-element 0)) SIZE))
(define unew (make-array (for/vector ([i SIZE]) (make-element 0)) SIZE))

(define (main)
  (begin
    (when (> (/ (* DIFF dt) dxdx) 0.5)
      (print "configuration error\n")
      )
    (define thread (list THREADX THREADY 1))
    (define block (list BLOCKX BLOCKY 1))
    (invoke-kernel init block thread u ulap unew)
    (invoke-kernel laplacian block thread u ulap)
    (invoke-kernel integrate block thread u ulap unew)
    (invoke-kernel update block thread u unew)
    ))

(define (main-s)
  (begin
    (when (> (/ (* DIFF dt) dxdx) 0.5)
      (print "configuration error\n")
      )
    (define thread (list THREADX THREADY 1))
    (define block (list BLOCKX BLOCKY 1))
    (invoke-kernel init-s block thread u-s ulap unew)
    (invoke-kernel laplacian-s block thread u-s ulap)
    (invoke-kernel integrate-s block thread u-s ulap unew)
    (invoke-kernel update-s block thread u-s unew)
    ))

;; Add a constraint that it is equal to each of the elements of
;; two arrays, arr1 and arr2, to asserts.
(define (array-eq-verify arr1 arr2 len)
  (for ([i (in-range len)])
    (assert
     (eq?
      (array-ref-host arr1 i)
      (array-ref-host arr2 i)))))

(define (diffusion2d-verify)
  (time (verify
         (begin
           (main)
           (main-s)
           (array-eq-verify u u-s SIZE)))))

(define (diffusion2d-synth)
  (map syntax->datum
       (generate-forms
        (time (synthesize #:forall '()
                          #:guarantee (begin
                                        (main)
                                        (main-s)
                                        (array-eq-verify u u-s SIZE)))))))

(define-symbolic sin (~> real? real?))
(define (spec x) (sin (- x pi)))
(define (tar x) (sin ((choose - +) x pi)))
(define-symbolic x real?)
(define (synth)
    (time
     (map syntax->datum
          (generate-forms
           (synthesize #:forall (list x)
                       #:guarantee (assert (eq? (spec x) (tar x))))))))