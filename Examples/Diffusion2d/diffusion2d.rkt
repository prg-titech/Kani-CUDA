#lang rosette

(require "../../lang.rkt"
         (rename-in rosette/lib/synthax [choose ?])
         racket/hash)

(define M-PI 8)
(define Lx (* 2 M-PI))
(define Ly (* 2 M-PI))
(define Nx 16)
(define Ny 16)
(define dx (quotient Lx (- Nx 1)))
(define dy (quotient Ly (- Ny 1)))
(define dt 0.0001)
(define endT (1.0))
(define Nt (quotient endT dt))
(define DIFF (1.0))
(define dxdx (* dx dx))
(define dydy (* dy dy))
(define THREADX 8)
(define THREADY 8)
(define BLOCKX (quotient Nx THREADX))
(define BLOCKY (quotient Ny THREADY))
(define Nbytes (* Nx Ny 12))

(define (laplacian u ulap)
  (: int i j ij ip1 im1 jp1 jm1)
  (= i (+/LS (thread-idx 0) (*/LS (block-idx 0) (block-dim 0))))
  (= j (+/LS (thread-idx 1) (*/LS (block-idx 1) (block-dim 1))))
  
  (if (&&/LS (&&/LS (</LS 0 i) (</LS i (-/LS Nx 1))) (&&/LS (</LS 0 j) (</LS j (-/LS Ny 1))))
      (begin 
        (= ij (+/LS i (*/LS Nx j)))
        (= ip1 (+/LS i 1 (*/LS Nx j)))
        (= im1 (+/LS i -1 (*/LS Nx j)))
        (= jp1 (+/LS i (*/LS Nx (+/LS j 1))))
        (= jm1 (+/LS i (*/LS Nx (-/LS j 1))))
        (= [ulap ij] (+/LS (//LS (+/LS [u ip1] (*/LS -2 [u ij]) [u jm1]) dxdx) (//LS (+/LS [u ip1] (*/LS -2 [u ij]) [u jm1]) dydy))))
      (void)
      )
  )

(define (integrate u ulap unew)
  (: int i j ij)
  (= i (+/LS (thread-idx 0) (*/LS (block-idx 0) (block-dim 0))))
  (= j (+/LS (thread-idx 1) (*/LS (block-idx 1) (block-dim 1))))
  (= ij (+/LS i (*/LS Nx j)))
  (= [unew ij] (+/LS [u ij] (*/LS dt [ulap ij])))
  )

(define (update u unew)
  (: int i j ij)
  (= i (+/LS (thread-idx 0) (*/LS (block-idx 0) (block-dim 0))))
  (= j (+/LS (thread-idx 1) (*/LS (block-idx 1) (block-dim 1))))
  (= ij (+/LS i (*/LS Nx j)))
  (= [u ij] [unew ij])
  )

(define (init u ulap unew)
  (: int i j ij)
  (: real x y)
  (= i (+/LS (thread-idx 0) (*/LS (block-idx 0) (block-dim 0))))
  (= j (+/LS (thread-idx 1) (*/LS (block-idx 1) (block-dim 1))))
  (= ij (+/LS i (*/LS Nx j)))
  
  (= x (*/LS i dx))
  (= x (*/LS j dy))
  (= [u ij] (*/LS (sin x) (sin y)))
  (= [unew ij] 0)
  (= [ulap ij] 0)
  )

(define (r)
  (define-symbolic* r real?)
  r)

(define SIZE (* Nx Ny)) 

;; Input array on CPU
(define CPU-in (make-array (for/vector ([i SIZE]) (make-element (r))) SIZE))
;; Input array on GPU
(define GPU-in (make-array (for/vector ([i SIZE]) (make-element (array-ref-host CPU-in i))) SIZE))
;; Output array on CPU
(define CPU-out (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
;; Output array on GPU
(define GPU-out (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))