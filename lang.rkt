#lang rosette

(require "array.rkt" "control.rkt" "work.rkt"
         "real.rkt" "operators.rkt" "barrier.rkt" "memory.rkt"
         rosette/lib/synthax)

(provide
 ;; syntax
 if- while : = := invoke-kernel
 ;; thread ID
 tid ntid
 ;; arithmetic/Boolean operators
 ;; /LS is for avoiding naming conflicts 
 +/LS -/LS eq?/LS !/LS &&/LS </LS >/LS quotient/LS
 ;; barrier
 barrier
 ;; kernel invocation
 invoke-kernel
 ;; queue
 choose generate-forms
 ;; objects
 array array-contents element-content make-element make-array
 ;; real type
 int
 ;; memory
 global-memory
 
 (all-from-out rosette/lib/synthax))

;; syntax
(define-syntax (if- stx)
  (syntax-case stx ()
    [(_ b then-cl else-cl)
     #'(if/LS (lambda () b)
              (lambda () then-cl)
              (lambda () else-cl))]
    [(_ b then-cl)
     #'(if- b then-cl (void))]))

(define-syntax (while stx)
  (syntax-case stx ()
    [(_ b #:bound bound body ...)
     #'(while-with-bound/LS (lambda () b) (lambda () body ...) bound)]
    [(_ b body ...)
     #'(while/LS (lambda () b) (lambda () body ...))]))

(define-syntax (: stx)
  (syntax-case stx ()
    [(_ type x ...)
     #'(begin (define x (new-vec type)) ...)]))

(define-syntax (:= stx)
  (syntax-case stx ()
    [(_ type x val)
     #'(begin
         (define x (new-vec type))
         (vec-set! x val))]))

(define-syntax (= stx)
  (syntax-case stx ()
    [(_ var exp)
     (identifier? #'var)
     #'(vec-set! var exp)]
    [(_ [arr idx] exp)
     #'(array-set! arr idx exp)]))

;; execute kernel
(define (invoke-kernel ker n . arg)
  (parameterize ([ntid n]
                 [mask (make-vector n #t)])
    (apply ker arg)))
