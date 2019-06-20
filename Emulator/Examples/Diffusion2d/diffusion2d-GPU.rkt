#lang rosette

(provide (all-defined-out))

(require "../../lang.rkt"
         "diffusion2d-h.rkt"
         (rename-in rosette/lib/synthax [choose ?])
         racket/hash)

(define (laplacian u ulap)
  (: int i j ij ip1 im1 jp1 jm1)
  (= i (+/LS (thread-idx 0) (*/LS (block-idx 0) (block-dim 0))))
  (= j (+/LS (thread-idx 1) (*/LS (block-idx 1) (block-dim 1))))
  
  (if- (&&/LS (&&/LS (</LS 0 i) (</LS i (-/LS Nx 1))) (&&/LS (</LS 0 j) (</LS j (-/LS Ny 1))))
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
  (= y (*/LS j dy))
  ;(= [u ij] (*/LS x y))
  (= [unew ij] 0.0)
  (= [ulap ij] 0.0)
  )