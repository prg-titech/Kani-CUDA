#lang racket

(provide host-translator)

(require c)

;; TODO __global__の処理を書く

(define (host-translator src)
  (cond
    [(type? src) (cond
                   [(type:primitive? src) (type:primitive-name src)]
                   [(type:function? src) (type:function-formals src)]
                   [(type:array? src) (host-translator (type:array-length src))]
                   [(type:qualified? src) (host-translator (type:qualified-type src))])]
    [(id? src) (cond
                 [(id:var? src) (let ([name (id:var-name src)])
                                  (cond [(eq? (id:var-name src) 'threadIdx) 'thread-idx]
                                        [(eq? (id:var-name src) 'blockIdx) 'block-idx]
                                        [(eq? (id:var-name src) 'blockDim) 'block-dim]
                                        [(eq? (id:var-name src) 'cudaMemcpyDeviceToHost) 0]
                                        [(eq? (id:var-name src) 'cudaMemcpyHostToDevice) 1]
                                        [else name]))]                     
                 [(id:op? src) (cond
                                 [(eq? '= (id:op-name src)) 'set!]
                                 [else (id:op-name src)])]
                 [(id:label? src) (cond [(eq? (id:label-name src) 'x) 0]
                                        [(eq? (id:label-name src) 'y) 1]
                                        [(eq? (id:label-name src) 'z) 2])])]
    [(stmt? src) (cond
                   [(stmt:expr? src) (host-translator (stmt:expr-expr src))]
                   [(stmt:block? src) (for/list ([src (stmt:block-items src)])
                                        (host-translator src))]
                   [(stmt:if? src) (if (not (stmt:if-alt src)) (quasiquote
                                                                (if
                                                                 (unquote (host-translator (stmt:if-test src)))
                                                                 (unquote (host-translator (stmt:if-cons src)))))
                                       (quasiquote
                                        (if
                                         (unquote (host-translator (stmt:if-test src)))
                                         (unquote (host-translator (stmt:if-cons src)))
                                         (unquote (host-translator (stmt:if-alt src))))))]
                   [(stmt:for? src) (let ([init (stmt:for-init src)]
                                          [test (stmt:for-test src)]
                                          [update (stmt:for-update src)])
                                      (append
                                       (list 'for-)
                                       (list
                                        (append
                                         (if init
                                             (list (host-translator init) ':)
                                             (list ':))
                                         (list (host-translator test))
                                         (if update
                                             (list ': (host-translator update))
                                             (list ':))))
                                       (host-translator (stmt:for-body src))))]
                   [(stmt:return? src) (let ([res (stmt:return-result src)])
                                         (if res
                                             (host-translator res)
                                             0))])]
    [(expr? src) (cond
                   [(expr:int? src) (expr:int-value src)]
                   [(expr:float? src) (expr:float-value src)]
                   [(expr:unop? src) (if (eq? (host-translator (expr:unop-op src)) '&)
                                         (host-translator (expr:unop-expr src))
                                         (quasiquote
                                          ((unquote (host-translator (expr:unop-op src)))
                                           (unquote (host-translator (expr:unop-expr src))))))]
                   [(expr:binop? src) (quasiquote
                                       ((unquote (host-translator (expr:binop-op src)))
                                        (unquote (host-translator (expr:binop-left src)))
                                        (unquote (host-translator (expr:binop-right src)))))]
                   [(expr:assign? src) (if (expr:array-ref? (expr:assign-left src))
                                           (quasiquote
                                            (array-set-host!
                                             (unquote (host-translator (expr:array-ref-expr (expr:assign-left src))))
                                             (unquote (host-translator (expr:array-ref-offset (expr:assign-left src))))
                                             (unquote (host-translator (expr:assign-right src)))))
                                           (quasiquote
                                            ((unquote (host-translator (expr:assign-op src)))
                                             (unquote (host-translator (expr:assign-left src)))
                                             (unquote (host-translator (expr:assign-right src))))))]
                   [(expr:member? src) (quasiquote
                                        ((unquote (host-translator (expr:member-expr src)))
                                         (unquote (host-translator (expr:member-label src)))))]
                   [(expr:ref? src) (host-translator (expr:ref-id src))]
                   [(expr:array-ref? src) (quasiquote
                                           (array-ref-host
                                            (unquote (host-translator (expr:array-ref-expr src)))
                                            (unquote (host-translator (expr:array-ref-offset src)))))]
                   [(expr:call? src) (let ([name (host-translator (expr:call-function src))])
                                       (cond [(or (eq? name 'block) (eq? name 'grid))
                                              (list
                                               'define
                                               name
                                               (list*
                                                'list
                                                (for/list
                                                    ([arg (expr:call-arguments src)])
                                                  (host-translator arg))))]
                                             [(string-contains? (symbol->string name) "__global__")
                                              (list* 'invoke-kernel
                                                     name
                                                     (for/list
                                                         ([arg (expr:call-arguments src)])
                                                       (host-translator arg)))]
                                             [else (list*
                                                    name
                                                    (for/list
                                                        ([arg (expr:call-arguments src)])
                                                      (host-translator arg)))]))]
                   [(expr:postfix? src) (list
                                         (host-translator (expr:postfix-op src))
                                         (host-translator (expr:postfix-expr src)))]
                   [(expr:prefix? src) (list
                                        (host-translator (expr:prefix-op src))
                                        (host-translator (expr:prefix-expr src)))]
                   [(expr:sizeof? src) '1]
                   [(expr:cast? src) (host-translator (expr:cast-expr src))])]
    [(decl? src) (cond
                   [(decl:vars? src)
                    (let ([decls (decl:vars-declarators src)])
                      (list*
                       'begin
                       (for/list ([decl decls])
                         (let ([init (decl:declarator-initializer decl)]
                               [type (decl:declarator-type decl)])
                           (if (type:array? type)
                               (if init
                                   (quasiquote
                                    (:= (unquote (host-translator (decl:vars-type src)))
                                        [(unquote (host-translator (decl:declarator-id decl)))
                                         (unquote (host-translator type))]
                                        (unquote (host-translator (init:expr-expr init)))))
                                   (quasiquote
                                    (: (unquote (host-translator (decl:vars-type src)))
                                       [(unquote (host-translator (decl:declarator-id decl)))
                                        (unquote (host-translator type))])))
                               (if (type:pointer? type)
                                   (if init
                                       (quasiquote
                                        (:= (unquote (host-translator (decl:vars-type src)))
                                            (unquote (host-translator (decl:declarator-id decl)))
                                            (unquote (host-translator (init:expr-expr init)))))
                                       (quasiquote
                                        (:* (unquote (host-translator (decl:vars-type src)))
                                            (unquote (host-translator (decl:declarator-id decl))))))
                                   (if init
                                       (quasiquote
                                        (define
                                          (unquote (host-translator (decl:declarator-id decl)))
                                          (unquote (host-translator (init:expr-expr init)))))
                                       (quasiquote
                                        (define 
                                          (unquote (host-translator (decl:declarator-id decl)))
                                          0)))))))))]
                   [(decl:function? src) (append
                                          (list 'func
                                                (host-translator (decl:function-return-type src))
                                                (host-translator (decl:declarator-id (decl:function-declarator src)))
                                                (for/list ([arg (host-translator (decl:declarator-type (decl:function-declarator src)))])
                                                  (host-translator arg)))
                                          (host-translator (decl:function-body src)))]
                   [(decl:formal? src) (list
                                        (host-translator (decl:formal-type src))
                                        (host-translator (decl:declarator-id (decl:formal-declarator src))))])]
    ))

