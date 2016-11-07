#lang rosette

(require "array.rkt" "control.rkt" "work.rkt"
         "real.rkt" "operators.rkt" "barrier.rkt" "memory.rkt"
         rosette/lib/synthax rosette/lib/angelic)

(provide
 ;; syntax
 if- while : = += := invoke-kernel :shared
 ;; thread ID
 thread-idx block-size
 ;; block ID
 block-idx
 ;; arithmetic/Boolean operators
 ;; /LS is for avoiding naming conflicts 
 +/LS -/LS */LS eq?/LS !/LS &&/LS </LS >/LS quotient/LS modulo/LS
 ;; barrier
 barrier
 ;; kernel invocation
 invoke-kernel
 ;; queue
 choose choose* generate-forms
 ;; objects
 array array-contents element-content make-element make-array
 ;; real type
 int
 ;; memory
 global-memory
 ;; option
 printmatrix
 ;; function for host
 array-ref-host array-set-host!
 
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

(define-syntax (:shared stx)
  (syntax-case stx ()
    [(_ type arr[n])
     #'(define arr (make-shared-array type n))]
    [(_ type arr[n][m])
     #'(define arr (make-shared-array type n m))]))

(define-syntax (:= stx)
  (syntax-case stx ()
    [(_ type x val)
     #'(begin
         (define x (new-vec type))
         (vec-set! x val))]))

(define-syntax (+= stx)
  (syntax-case stx ()
    [(_ var exp)
     #'(vec-set! var (+/LS var exp))]))

(define-syntax (= stx)
  (syntax-case stx ()
    [(_ var exp)
     (identifier? #'var)
     #'(vec-set! var exp)]
    [(_ [arr idx ...] exp)
     #'(array-set-dim! arr exp idx ...)]))

;; execute kernel
(define (invoke-kernel
         ker   ; string
         gdim  ; list of int
         bdim  ; list of int
         . arg); any
  (parameterize* ([grid-dimension gdim]
                  [block-dimension bdim]
                  [shared-memory (make-shared-memory (block-size))])
    (for ([b (in-range (grid-size))])
      (parameterize* ([bid b]
                      [block-index (to-bid b)]
                      [mask (make-vector (block-size) #t)])
        (apply ker arg)))))
