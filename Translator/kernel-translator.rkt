#lang racket

(provide (all-defined-out))

(require c)

;; TODO: Implement ternary op

(define (get-name src)
  (if (decl:function? src)
      (kernel-translator (decl:declarator-id (decl:function-declarator src)))
      'xxx))

(define (kernel-translator src)
  (cond
    [(type? src) (cond
                   [(type:primitive? src) (type:primitive-name src)]
                   [(type:function? src) (type:function-formals src)]
                   [(type:array? src) (kernel-translator (type:array-length src))]
                   [(type:qualified? src) (kernel-translator (type:qualified-type src))])]
    [(id? src) (cond
                 [(id:var? src) (let ([name (id:var-name src)])
                                  (cond [(eq? (id:var-name src) 'threadIdx) 'thread-idx]
                                        [(eq? (id:var-name src) 'blockIdx) 'block-idx]
                                        [(eq? (id:var-name src) 'blockDim) 'block-dim]
                                        [(eq? (id:var-name src) 'cudaMemcpyDeviceToHost) 0]
                                        [(eq? (id:var-name src) 'cudaMemcpyHostToDevice) 1]
                                        [else name]))]                     
                 [(id:op? src) (if (eq? '= (id:op-name src))
                                   (id:op-name src)
                                   (string->symbol
                                    (string-append
                                     (symbol->string
                                      (id:op-name src))
                                     "/LS")))]
                 [(id:label? src) (cond [(eq? (id:label-name src) 'x) 0]
                                        [(eq? (id:label-name src) 'y) 1]
                                        [(eq? (id:label-name src) 'z) 2])])]
    [(stmt? src) (cond
                   [(stmt:expr? src) (kernel-translator (stmt:expr-expr src))]
                   [(stmt:block? src) (for/list ([src (stmt:block-items src)])
                                        (kernel-translator src))]
                   [(stmt:if? src) (if (not (stmt:if-alt src))
                                       (quasiquote
                                        (if-
                                         (unquote (kernel-translator (stmt:if-test src)))
                                         (begin (unquote (kernel-translator (stmt:if-cons src))))))
                                       (quasiquote
                                        (if-
                                         (unquote (kernel-translator (stmt:if-test src)))
                                         (begin
                                           (unquote (kernel-translator (stmt:if-cons src))))
                                         (begin
                                           (unquote (kernel-translator (stmt:if-alt src)))))))]
                   [(stmt:for? src) (let ([init (stmt:for-init src)]
                                          [test (stmt:for-test src)]
                                          [update (stmt:for-update src)])
                                      (append
                                       (list 'for-)
                                       (list
                                        (append
                                         (if init
                                             (list (kernel-translator init) ':)
                                             (list ':))
                                         (list (kernel-translator test))
                                         (if update
                                             (list ': (kernel-translator update))
                                             (list ':))))
                                       (kernel-translator (stmt:for-body src))))]
                   [(stmt:return? src) (let ([res (stmt:return-result src)])
                                         (if res
                                             (kernel-translator res)
                                             0))])]
    [(expr? src) (cond
                   [(expr:int? src) (expr:int-value src)]
                   [(expr:float? src) (expr:float-value src)]
                   [(expr:unop? src) (quasiquote
                                      ((unquote (kernel-translator (expr:unop-op src)))
                                       (unquote (kernel-translator (expr:unop-expr src)))))]
                   [(expr:binop? src) (quasiquote
                                       ((unquote (kernel-translator (expr:binop-op src)))
                                        (unquote (kernel-translator (expr:binop-left src)))
                                        (unquote (kernel-translator (expr:binop-right src)))))]
                   [(expr:assign? src) (quasiquote
                                        ((unquote (kernel-translator (expr:assign-op src)))
                                         (unquote (kernel-translator (expr:assign-left src)))
                                         (unquote (kernel-translator (expr:assign-right src)))))]
                   [(expr:member? src) (quasiquote
                                        ((unquote (kernel-translator (expr:member-expr src)))
                                         (unquote (kernel-translator (expr:member-label src)))))]
                   [(expr:ref? src) (kernel-translator (expr:ref-id src))]
                   [(expr:array-ref? src) (quasiquote
                                           [(unquote (kernel-translator (expr:array-ref-expr src)))
                                            (unquote (kernel-translator (expr:array-ref-offset src)))])]
                   [(expr:call? src) (list*
                                      (kernel-translator (expr:call-function src))
                                      (for/list
                                          ([arg (expr:call-arguments src)])
                                        (kernel-translator arg)))]
                   [(expr:postfix? src) (list
                                         (kernel-translator (expr:postfix-op src))
                                         (kernel-translator (expr:postfix-expr src)))]
                   [(expr:prefix? src) (list
                                        (kernel-translator (expr:prefix-op src))
                                        (kernel-translator (expr:prefix-expr src)))]
                   [(expr:sizeof? src) '1]
                   [(expr:cast? src) (kernel-translator (expr:cast-expr src))])]
    [(decl? src) (cond [(decl:vars? src) (let ([decls (decl:vars-declarators src)])
                                           (list*
                                            'begin
                                            (for/list ([decl decls])
                                              (let ([init (decl:declarator-initializer decl)]
                                                    [type (decl:declarator-type decl)])
                                                (if (type:array? type)
                                                    (if init
                                                        (quasiquote
                                                         (:= (unquote (kernel-translator (decl:vars-type src)))
                                                             [(unquote (kernel-translator (decl:declarator-id decl)))
                                                              (unquote (kernel-translator type))]
                                                             (unquote (kernel-translator (init:expr-expr init)))))
                                                        (quasiquote
                                                         (: (unquote (kernel-translator (decl:vars-type src)))
                                                            [(unquote (kernel-translator (decl:declarator-id decl)))
                                                             (unquote (kernel-translator type))])))
                                                    (if (type:pointer? type)
                                                        (if init
                                                            (quasiquote
                                                             (:= (unquote (kernel-translator (decl:vars-type src)))
                                                                 (unquote (kernel-translator (decl:declarator-id decl)))
                                                                 (unquote (kernel-translator (init:expr-expr init)))))
                                                            (quasiquote
                                                             (:* (unquote (kernel-translator (decl:vars-type src)))
                                                                 (unquote (kernel-translator (decl:declarator-id decl))))))
                                                        (if init
                                                            (quasiquote
                                                             (:= (unquote (kernel-translator (decl:vars-type src)))
                                                                 (unquote (kernel-translator (decl:declarator-id decl)))
                                                                 (unquote (kernel-translator (init:expr-expr init)))))
                                                            (quasiquote
                                                             (: (unquote (kernel-translator (decl:vars-type src)))
                                                                (unquote (kernel-translator (decl:declarator-id decl))))))))))))]
                       [(decl:function? src) (append
                                              (list 'define
                                                    (list*
                                                     (kernel-translator (decl:declarator-id (decl:function-declarator src)))
                                                     (for/list ([arg (kernel-translator (decl:declarator-type (decl:function-declarator src)))])
                                                       (kernel-translator arg))))
                                              (kernel-translator (decl:function-body src)))]
                       [(decl:formal? src) (kernel-translator (decl:declarator-id (decl:formal-declarator src)))])]
    ))