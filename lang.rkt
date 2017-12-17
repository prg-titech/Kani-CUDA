#lang rosette

(require "array.rkt" "control.rkt" "work.rkt"
         "real.rkt" "operators.rkt" "barrier.rkt"
         "memory.rkt"
         rosette/lib/synthax rosette/lib/angelic
         (rename-in rosette/lib/synthax [choose ?])
         racket/hash)

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
 syncthreads
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
 
 optimize-barrier
 barrier?
 ?
 write-synth-result
 switch
 
 (all-from-out rosette/lib/synthax racket/hash))

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

;; A function to execute kernel
(define (invoke-kernel
         kernel ; procedure/function
         gdim  ; list of int
         bdim  ; list of int
         . arg); any
  (clear-bc)
  (parameterize* ([grid-dimension gdim]
                  [block-dimension bdim]
                  [shared-memory (make-shared-memory (grid-size))])
    (for ([b (in-range (grid-size))])
      (parameterize* ([bid b]
                      [block-index (to-bid b)]
                      [mask (make-vector (block-size) #t)])
        (apply kernel arg)
        (barrier)
        (barrier/B)))))

;; A function that synthesize so as to minimize the number of barrier
;; TODO Correspond when synthesizing multiple codes
;; guarantee :: M -> ()
(define (optimize-barrier guarantee)
  (define res
    (list-ref
     (map
      syntax->datum
      (generate-forms (optimize
                       #:minimize (list (get-bc))
                       #:guarantee guarantee)))
     0))
  
  (define (replace-barrier lst)
    (for/list ([e lst])
      (cond
        [(! (list? e)) e]
        [(eq? e '(if (switch) (void) (syncthreads))) null]
        [(eq? e '(if (switch) (syncthreads) (syncthreads))) '(syncthreads)]
        [(member (list-ref e 0) (list 'if 'if- 'while 'for-)) (replace-barrier e)]
        [else e])))
  (filter (lambda (e) (not (eq? e null)))
          (replace-barrier (list-ref res 2)))
  )

(define (rewrite-name name)
  (string->symbol (string-append* "res-" (list (symbol->string name)))))

(define (function res)
  (list-ref (cdr res) 0))

(define (function-name res)
  (list-ref (function res) 0))

(define (body res)
  (cdr (cdr res)))

(define (write-synth-result res test)
  (define out (open-output-file "res.rkt"
                                #:exists 'truncate))
  
  (fprintf out "#lang rosette\n")
  (for-each (lambda (e)
              (writeln e out))
            (list '(require "../../lang.rkt")
                  (list 'define 'res-f
                        (quasiquote
                         (unquote (append (list 'lambda)
                                          (list (cdr (function res)))
                                          (body res)))))))
  (for-each (lambda (e)
              (writeln e out))
            (quasiquote (unquote test)))
  
  (writeln '(for-each (lambda (e)
                        (displayln e))
                      (optimize-barrier (parameterize ([switch #t])
                                          (spec-rotate-opt res-f)))) out)
  (close-output-port out)
  )

(define switch (make-parameter #t))

(define (r)
  (define-symbolic* x boolean?)
  x)

(define (barrier?)
  (if (switch)
      (let ([r (r)])
        (begin
          (displayln r)
          (if r (void) (syncthreads))))
      (syncthreads)))