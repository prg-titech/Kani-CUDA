#lang rosette

(provide (all-defined-out))

(require "../../lang.rkt"
         "diffusion2d-h.rkt"
         (rename-in rosette/lib/synthax [choose ?])
         racket/hash)

(define (laplacian-s u ulap)
  (: int i j ij tx ty)
  (:shared real su[(+ 2 THREADX)][(+ 2 THREADY)])
  
  (= i (+/LS (thread-idx 0) (*/LS (block-idx 0) (block-dim 0))))
  (= j (+/LS (thread-idx 1) (*/LS (block-idx 1) (block-dim 1))))
  (= tx (+/LS (thread-idx 0) 1))
  (= ty (+/LS (thread-idx 1) 1))
  
  (= ij (+/LS i (*/LS Nx j)))
  (= [su tx ty] [u ij])
  (barrier)
  
  (if- (&&/LS (eq?/LS 0 (block-idx 0)) (eq?/LS 0 (thread-idx 0)))
       (= [su (-/LS tx 1) ty] (-/LS (*/LS 2 [su tx ty]) [su (+/LS tx 1) ty])))
  (if- (&&/LS (neq?/LS 0 (block-idx 0)) (eq?/LS 0 (thread-idx 0)))
       (= [su (-/LS tx 1) ty] [u (+/LS (*/LS j Nx) i -1)]))
  (if- (&&/LS (eq?/LS (-/LS (grid-dim 0) 1) (block-idx 0)) (eq?/LS (-/LS (block-dim 0) 1) (thread-idx 0)))
       (= [su (+/LS tx 1) ty] (-/LS (*/LS 2 [su tx ty]) [su (-/LS tx 1) ty])))
  (if- (&&/LS (neq?/LS (-/LS (grid-dim 0) 1) (block-idx 0)) (eq?/LS (-/LS (block-dim 0) 1) (thread-idx 0)))
       (= [su (+/LS tx 1) ty] [u (+/LS (*/LS j Nx) i 1)]))
  (if- (&&/LS (eq?/LS 0 (block-idx 1)) (eq?/LS 0 (thread-idx 1)))
       (= [su tx (-/LS ty 1)] (-/LS (*/LS 2 [su tx ty]) [su tx (+/LS ty 1)])))
  (if- (&&/LS (neq?/LS 0 (block-idx 1)) (eq?/LS 0 (thread-idx 1)))
       (= [su tx (-/LS ty 1)] [u (+/LS (*/LS (-/LS j 1) Nx) i)]))
  (if- (&&/LS (eq?/LS (-/LS (grid-dim 1) 1) (block-idx 1)) (eq?/LS (-/LS (block-dim 1) 1) (thread-idx 1)))
       (= [su tx (+/LS ty 1)] (-/LS (*/LS 2 [su tx ty]) [su tx (-/LS ty 1)])))
  (if- (&&/LS (neq?/LS (-/LS (grid-dim 1) 1) (block-idx 1)) (eq?/LS (-/LS (block-dim 1) 1) (thread-idx 1)))
       (= [su tx (+/LS ty 1)] [u (+/LS (*/LS (+/LS j 1) Nx) i)]))
  
  (barrier)
  (= [ulap ij] (+/LS (//LS (+/LS [su (-/LS tx 1) ty] (*/LS -2 [su tx ty]) [su (+/LS tx 1) ty]) dxdx) (//LS (+/LS [su tx (-/LS ty 1)] (*/LS -2 [su tx ty]) [su tx (+/LS ty 1)]) dydy)))
  )

(define (integrate-s u ulap unew)
  (: int i j ij)
  (= i (+/LS (thread-idx 0) (*/LS (block-idx 0) (block-dim 0))))
  (= j (+/LS (thread-idx 1) (*/LS (block-idx 1) (block-dim 1))))
  (= ij (+/LS i (*/LS Nx j)))
  (= [unew ij] (+/LS [u ij] (*/LS dt [ulap ij])))
  )

(define (update-s u unew)
  (: int i j ij)
  (= i (+/LS (thread-idx 0) (*/LS (block-idx 0) (block-dim 0))))
  (= j (+/LS (thread-idx 1) (*/LS (block-idx 1) (block-dim 1))))
  (= ij (+/LS i (*/LS Nx j)))
  (= [u ij] [unew ij])
  )

(define (init-s u ulap unew)
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