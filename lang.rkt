#lang rosette

(require "array.rkt" "control.rkt" "work.rkt"
         "real.rkt" "operators.rkt" ;;"barrier.rkt"
         "memory.rkt"
         rosette/lib/synthax rosette/lib/angelic)

(provide
 mask
 ;; Syntax
 ;; Control statement
 if- while for-
 ;; Variable declaration
 : := :shared
 ;; Assignment operators
 += = -- ++
 ;; Thread ID
 thread-idx
 ;; Function on the block
 block-idx block-size block-dim
 ;; Function on the grid
 grid-size grid-dim
 ;; Arithmetic/Boolean operators
 ;; /LS is for avoiding naming conflicts
 +/LS -/LS */LS //LS
 eq?/LS !/LS &&/LS ||/LS </LS >/LS
 sin/LS cos/LS neq?/LS
 quotient/LS modulo/LS
 max/LS min/LS
 ;; Ternary operator
 ?:
 ;; Barrier
 barrier
 ;; Kernel invocation
 invoke-kernel
 ;; Synthesis library
 choose generate-forms
 ;; Real type
 int real bool
 ;; Memory
 global-memory
 ;; Option
 print-matrix
 ;; Function/procedure for host
 array-ref-host array-set-host!
 make-element make-array
 
 (all-from-out rosette/lib/synthax))

;; Kernel syntax
(define-syntax (if- stx)
  (syntax-case stx ()
    [(_ b then-cl else-cl)
     #'(if/LS (lambda () b)
              (lambda () then-cl)
              (lambda () else-cl))]
    [(_ b then-cl)
     #'(if- b then-cl (void))]))

(define-syntax (?: stx)
  (syntax-case stx ()
    [(_ b then-ex else-ex)
     #'(?:/LS (lambda () b)
              (lambda () then-ex)
              (lambda () else-ex))]))

(define-syntax (while stx)
  (syntax-case stx ()
    [(_ b #:bound bound body ...)
     #'(while-with-bound/LS (lambda () b) (lambda () body ...) bound)]
    [(_ b body ...)
     #'(while/LS (lambda () b) (lambda () body ...))]))

(define-syntax (for- stx)
  (syntax-case stx (:)
    [(_ [init : cond : change] body ...)
     #'(for/LS
           (lambda () init)
         (lambda () cond)
         (lambda () change)
         (lambda () body ...))]
    [(_ [: cond : change] body ...)
     #'(for/LS
           void
         (lambda () cond)
         (lambda () change)
         (lambda () body ...))]))


(define-syntax (: stx)
  (syntax-case stx ()
    [(_ type x ...)
     #'(begin (define x (new-vec type)) ...)]
    [(_ type arr[n])
     #'(begin
         (define (t)
           (define-symbolic* t type)
           t)
         (define arr
           (make-array
            (for/vector ([i n])
              (make-element (t)))
            n)))]))

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

(define-syntax (++ stx)
  (syntax-case stx ()
    [(_ var)
     #'(vec-set! var (+/LS var 1))]))

(define-syntax (-- stx)
  (syntax-case stx ()
    [(_ var)
     #'(vec-set! var (-/LS var 1))]))

(define-syntax (= stx)
  (syntax-case stx ()
    [(_ var exp)
     (identifier? #'var)
     #'(vec-set! var exp)]
    [(_ [arr idx ...] exp)
     #'(array-set-dim! arr exp idx ...)]))

;; Execute kernel
(define (invoke-kernel
         kernel ; procedure/function
         gdim  ; list of int
         bdim  ; list of int
         . arg); any
  (parameterize* ([grid-dimension gdim]
                  [block-dimension bdim]
                  [shared-memory (make-shared-memory (grid-size))])
    (for ([b (in-range (grid-size))])
      (parameterize* ([bid b]
                      [block-index (to-bid b)]
                      [mask (make-vector (block-size) #t)])
        (apply kernel arg)))
    (barrier)
    (barrier/B)))

;(define b (? (vecfy 1) (vecfy 2)))
;(define a (? 1 2))
;(define c (vecfy a))