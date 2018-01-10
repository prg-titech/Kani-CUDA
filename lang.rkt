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
 ?
 write-synth-result
 switch
 synth-with-kani-cuda
 
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
    [(_ type arr[n])
     #'(begin
         (define (t)
           (define-symbolic* t type)
           t)
         (define arr
           (make-array
            (for/vector ([i n])
              (make-element (t)))
            n)))]
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

;; A function to execute a Kani-CUDA kernel
;; kernel :: 
;; gdim :: list?
;; bdim :: list?
;; arf :: any/c
(define (invoke-kernel
         kernel
         gdim
         bdim  
         . arg)
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
        [(member (list-ref e 0) (list 'if 'if- 'while 'for- 'begin)) (replace-barrier e)]
        [else e])))
  
  
  (define (delete-null lst)
    (filter (lambda (e) (not (eq? e null)))
            (for/list ([e lst])
              (cond
                [(! (list? e)) e]
                [(null? e) e]
                [(member (list-ref e 0) (list 'if 'if- 'while 'for- 'begin)) (delete-null e)]
                [else e]))))

  (delete-null (replace-barrier (list-ref res 2)))
  )

;; A function that write a result of first synthesis to 'res.rkt'
;; res :: list?
;; test :: list?
(define (write-synth-result res test)
  (define (function res)
    (list-ref (cdr res) 0))
  
  (define (body res)
    (cdr (cdr res)))
  
  (define out (open-output-file "../../res.rkt"
                                #:exists 'truncate))
  
  (fprintf out "#lang rosette\n")
  (for-each (lambda (e)
              (pretty-write e out)
              (newline out))
            (list '(require "lang.rkt")
                  (list 'define 'res-f
                        (quasiquote
                         (unquote (append (list 'lambda)
                                          (list (cdr (function res)))
                                          (body res)))))))
  
  (define stmt 0)
  (define in-test (open-input-file test))
  (set! stmt (read in-test))
  (while (not (eof-object? stmt))
         (pretty-write stmt out)
         (newline out)
         (set! stmt (read in-test)))
  (close-input-port in-test)
  
  (pretty-write '(for-each (lambda (e)
                        (pretty-display e))
                      (optimize-barrier (parameterize ([switch #t])
                                          (spec-opt res-f)))) out)
  (newline out)
  (close-output-port out)
  )

;; A global variable to sequential synthesis
(define switch (make-parameter #t))

(define (aux-insert-barrier lst)
  (add-between 
   (for/list ([e lst])
     (cond
       [(eq? (car e) 'begin)
        (cons 'begin (aux-insert-barrier (cdr e)))]
       [(member (car e) (list 'while 'for 'for-))
        (cons (car e) (cons (cadr e) (aux-insert-barrier (cddr e))))]
       [(member (car e) (list 'if 'if-))
        (cons
         (car e)
         (cons
          (cadr e)
          (cons
           (aux-insert-barrier (list (list-ref e 2)))
           (aux-insert-barrier (list (list-ref e 3))))))]
       [else
        e]))
   '(if (switch)
        (? (syncthreads) (void))
        (syncthreads))))


(define (insert-barrier def)
  (append
   (list 'define (cadr def))
   (aux-insert-barrier (cddr def))))

;; 
(define (synth-with-kani-cuda path spec test)
  
  (define in (open-input-file path))
  (read-line in)
  (define stmt #f)
  (define lst null)
  (set! stmt (read in))
  (while (not (eof-object? stmt))
         (cond [(eq? (car stmt) 'define) (set! lst (append lst (list (insert-barrier stmt))))]
               [(eq? (car stmt) 'require) (set! lst (append lst (list '(require "lang.rkt"))))]
               [else (set! lst (append lst (list stmt)))])
         (set! stmt (read in)))
  (close-input-port in)
  (define out (open-output-file "../../sketch.rkt" #:exists 'truncate))
  (fprintf out "#lang rosette\n")
  (for-each (lambda (e)
              (pretty-write e out)
              (newline out))
            lst)
  
  (define in-spec (open-input-file spec))
  (set! stmt (read in-spec))
  (while (not (eof-object? stmt))
         (pretty-write stmt out)
         (newline out)
         (set! stmt (read in-spec)))
  (close-input-port in-spec)
  
  (pretty-write (quasiquote (write-synth-result res (unquote test))) out)
  (newline out)
  
  (close-output-port out)
  
  (define path-to-racket-exe
    "/Applications/Racket v6.6/bin//racket")
  (define path-to-rosette-code1
    "/Users/akira/Masuhara Lab/Kani-CUDA/sketch.rkt")
  (define path-to-rosette-code2
    "/Users/akira/Masuhara Lab/Kani-CUDA/res.rkt")
  
  (system*
   path-to-racket-exe
   path-to-rosette-code1)
  
  (system*
   path-to-racket-exe
   path-to-rosette-code2))