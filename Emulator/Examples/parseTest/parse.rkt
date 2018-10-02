#lang racket

(require c c/parse)

(define (convert src)
  (cond
    [(type:primitive? src) (quasiquote (unquote (type:primitive-name src)))]
    [(id? src) (cond
                 [(id:var? src) (id:var-name src)]
                 [(id:op? src) (string->symbol
                                (string-append
                                 (symbol->string
                                  (id:op-name src))
                                 "/LS"))]
                 [(id:label? src) (cond [(eq? (id:label-name src) 'x) 0]
                                        [(eq? (id:label-name src) 'y) 1]
                                        [(eq? (id:label-name src) 'z) 2])])]
    [(expr? src) (cond
                   [(expr:int? src) (expr:int-value src)]
                   [(expr:binop? src) (quasiquote
                                       ((unquote (convert (expr:binop-op src)))
                                        (unquote (convert (expr:binop-left src)))
                                        (unquote (convert (expr:binop-right src)))))]
                   [(expr:assign? src) (quasiquote
                                       ((unquote (convert (expr:assign-op src)))
                                        (unquote (convert (expr:assign-left src)))
                                        (unquote (convert (expr:assign-right src)))))]
                   [(expr:member? src) (quasiquote
                                        ((unquote (convert (expr:member-expr src)))
                                         (unquote (convert (expr:member-label src)))))]
                   [(expr:ref? src) (convert (expr:ref-id src))]
                   [(expr:array-ref? src) (quasiquote
                                           [(unquote (convert (expr:array-ref-expr src)))
                                            (unquote (convert (expr:array-ref-offset src)))])]
                   [(expr:call? src) (quasiquote
                                      ((unquote (list*
                                                 (convert (expr:call-function src))
                                                 (for/list
                                                     ([arg (expr:call-arguments src)])
                                                   (convert arg))))))])]
    [(decl:vars? src) (begin
                        (define dec (list-ref (decl:vars-declarators src) 0))
                        (quasiquote (:=
                                     (unquote (convert (decl:vars-type src)))
                                     (unquote (convert (decl:declarator-id dec)))
                                     (unquote (convert (init:expr-expr (decl:declarator-initializer dec)))))))]
    ))

(convert (list-ref (parse-program "int i = threadIdx.x + blockDim.x * blockIdx.x;") 0))
(parse-program "int i = threadIdx.x + blockDim.x * blockIdx.x;")