#lang rosette

(require "../../lang.rkt"
         "diffusion2d-GPU.rkt"
         "diffusion2d-h.rkt"
         ;"diffusion2d-GPU-shared.rkt"(main
         (rename-in rosette/lib/synthax [choose ?])
         racket/hash)


(define (r)
  (define-symbolic* r real?)
  r)

(define SIZE (* Nx Ny)) 

(define u (make-array (for/vector ([i SIZE]) (make-element (r))) SIZE))
(define ulap (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
(define unew (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))

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