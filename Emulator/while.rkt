#lang rosette

;; C言語の対応表
;; T x; => (define x (new-vec T))
;; - NOTE: T x = e; を (define x e) と書いてはならない. xとeで vector が共有される可能性がある．
;;   代わりに (begin (define x (new-vec T)) (vec-set! x e)) と書く．
;; x = e; => (vec-set! x e)
;; threadIdx.x => (tid)
;; arr[ix]; => (array-ref arr ix)
;; arr[ix] = e => array-set! 
;; if, while -> if-, while
;; syncthreads -> barrier

(provide
 ;; syntax
 if- while : = :=
 ;; arithmetic/Boolean operators
 ;; /LS is for avoiding naming conflicts 
 +/LS eq?/LS !/LS &&/LS </LS >/LS quotient/LS
 ;; thread ID
 ntid tid
 ;; vector/array
 new-vec scalar->vec new-sh-array vec-set! array-ref! array-set! make-element element-content array array-contents
 ;; barrier
 barrier
 ;; kernel invocation
 invoke-kernel
 ;; real type
 int
 )

;; preambles for executing the program in pure racket
;; (define ! not)
;; (define (&& x y) (and x y))
;; (define-syntax (define-symbolic* stx)
;;   (syntax-case stx ()
;;     [(_ var type) #'(define var 0)]))

;; number of threads and mask
;; before running the kernel, ntid need to be specified with a number: 
;;   (parameterize ([ntid ?]) (kernel))
(define ntid (make-parameter 16))
;; mask is only internally used  
(define mask (make-parameter (make-vector (ntid) #t)))

; element of array 
(struct element
  ([content #:mutable] ;int or boolean
   [read #:mutable]    ;int or boolean
   [write #:mutable]) ;int or boolean
  #:property prop:procedure
  (lambda (elem) (element-content elem)))

; make new element
(define (make-element e)
  (element e #f #f))

; array
; type check?
(struct array
  ([contents #:mutable])
  ;  #:property prop:procedure
  ;  (lambda (arr) (array-contents arr))
  #:property prop:procedure
  (lambda (arr idx)
    (array-ref! (array-contents arr) idx)))

;; convert a scalar value to a vector value
(define (vecfy x)
  ;(printf "vecfy x = ~a\n" x)
  (cond [(or (integer? x) (boolean? x)) (make-vector (ntid) x)]
        [(vector? x) x]
        [else (raise "vecfy: expected an integer/boolean or a vector")]))

;; map, zipWith
;; 'masked-value は mask されたスレッドが返す値を表し，map, zipWith は 'masked-value を無視する
(define (zipWith-vec f xs ys)
  (for/vector ([x xs]
               [y ys])
    (if (or (eq? x 'masked-value) (eq? y 'masked-value)) 'masked-value
        (f x y))))

(define (map-vec f xs)
  (for/vector ([x xs])
    (if (eq? x 'masked-value) 'masked-value
        (f x))))

;; lifting an operator on scalar values to an operator on vector
(define (LSop op)
  (lambda (x)
    (let ([x (vecfy x)])
      (map-vec op x))))

(define (LSop2 op)
  (lambda (x y)
    (let ([x (vecfy x)]
          [y (vecfy y)])
      (zipWith-vec op x y))))

;; lifting basic operators
(define +/LS (LSop2 +))
(define eq?/LS (LSop2 eq?))
(define !/LS (LSop !))
(define &&/LS (LSop2 &&))
(define >/LS (LSop2 >))
(define </LS (LSop2 <))
(define quotient/LS (LSop2 quotient))

;; thread id
(define (tid) (for/vector ([i (in-range (ntid))]) i))

;; make a symbolic vector with length ``n'' and type ``type''
(define (new-symbolic-vector n type)
  (for/vector ([i (in-range n)])
    (define-symbolic* x type)
    x))

;; create a new vector value with type ``type''
(define (new-vec type)
  (new-symbolic-vector (ntid) type))

;; create a scalar value from scalar ``s''
(define (scalar->vec s)
  (make-vector (ntid) s))

;; create a new array with length n and type ``type''
(define (new-sh-array n type)
  (new-symbolic-vector n type))

;; denotation of the statement ``xs = vs''
;; assign each element of vs to xs, except masked values
(define (vec-set! xs vs)
  (for ([i (in-range (ntid))]
        [m (mask)]
        [v (vecfy vs)])
    (when m (vector-set! xs i v))))

;; TODO; implement Read/Write set
;; denotation of an expression arr[ixs]
;; if a thread is masked, array-ref! returns the special symbol 'masked-value
(define (array-ref! arr ixs)
  (for/vector ([tid (tid)] 
               [i (vecfy ixs)]
               [m (mask)])
    (if m (let* ([elem (vector-ref arr i)]
                 [cont (element-content elem)]
                 [read (element-read elem)]
                 [write (element-write elem)])
            (if (or (eq? write tid) (eq? write #f))
                (begin
                  (set! read tid)
                  cont)
                (raise "array-ref!: expected an conflict")))
        'masked-value)))

;; denotation of the statement arr[ixs] = vs
;; array-set! assigns vs to each elements of arr[ixs]
(define (array-set! arr ixs vs)
  (for ([tid (tid)]
        [m (mask)]
        [i ixs]
        [v (vecfy vs)])
    ;;(printf "m, i, v = ~a, ~a, ~a\n" m i v)
    (when m
      (let* ([elem (vector-ref arr i)]
             [cont (element-content elem)]
             [read (element-read elem)]
             [write (element-write elem)])
        (if (or (eq? write tid) (eq? write #f) (eq? read tid) (eq? read #f))
            (begin
              (set! read tid)
              (set! write tid)
              (vector-set! arr i (make-element v)))
            (raise "array-set!: expected an conflict"))))))

;; denotation of if (b) {then-cl} {else-cl}
;; execute each clause with additional masks by b
;; then-cl, else-cl :: M -> ()
;; b :: () -> boolean
(define (if/LS b then-cl else-cl)
  (let ([bval (b)])
    (parameterize ([mask (&&/LS bval (mask))]) (then-cl))
    (parameterize ([mask (&&/LS (!/LS bval) (mask))]) (else-cl))))

;; denotation of while (b) {body}
;; execute body with addtional mask by b until all threads are masked
;; b :: () -> boolean? 
;; body :: M -> ()
(define (while/LS b body)
  (let* ([bval (b)]
         [m (&&/LS bval (mask))])
    (when (for/or ([v m]) v) ;; check that whether there are any non-masked thread
      (parameterize ([mask m])
        (body)
        (while/LS b body)))))

;; barrier divergence check
;; when the execution reach a barrier, we need to check that all 
;; threads are participate in this barrier
(define (barrier-ok m)
  (or (for/and ([x m]) x)
      (for/and ([x m]) (! x))))

;; barrier
;; just do the barrier divergence check
(define (barrier) 
  (let ([m (mask)])
    (assert (barrier-ok m))))

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
     #'(array-set! (array-contents arr) idx exp)]))

(define int integer?)

(define (invoke-kernel ker n . arg)
  (parameterize ([ntid n]
                 [mask (make-vector n #t)])
    (apply ker arg)))
