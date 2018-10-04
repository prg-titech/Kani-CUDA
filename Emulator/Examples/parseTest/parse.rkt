#lang racket

(require c c/parse)

(define (convert src)
  (cond
    [(type? src) (cond
                   [(type:primitive? src) (type:primitive-name src)]
                   [(type:function? src) (type:function-formals src)]
                   [(type:array? src) (convert (type:array-length src))])]
    [(id? src) (cond
                 [(id:var? src) (let ([name (id:var-name src)])
                                  (cond [(eq? (id:var-name src) 'threadIdx) 'thread-idx]
                                        [(eq? (id:var-name src) 'blockIdx) 'block-idx]
                                        [(eq? (id:var-name src) 'blockDim) 'block-dim]
                                        [else name]))]                     
                 [(id:op? src) (string->symbol
                                (string-append
                                 (symbol->string
                                  (id:op-name src))
                                 "/LS"))]
                 [(id:label? src) (cond [(eq? (id:label-name src) 'x) 0]
                                        [(eq? (id:label-name src) 'y) 1]
                                        [(eq? (id:label-name src) 'z) 2])])]
    [(stmt? src) (cond
                   [(stmt:expr? src) (convert (stmt:expr-expr src))]
                   [(stmt:block? src) (for/list ([src (stmt:block-items src)])
                                        (convert src))]
                   [(stmt:if? src) (if (not (stmt:if-alt src)) (quasiquote
                                                                (if-
                                                                 (unquote (convert (stmt:if-test src)))
                                                                 (unquote (convert (stmt:if-cons src)))))
                                       (quasiquote
                                        (if-
                                         (unquote (convert (stmt:if-test src)))
                                         (unquote (convert (stmt:if-cons src)))
                                         (unquote (convert (stmt:if-alt src))))))]
                   [(stmt:for? src) (let ([init (stmt:for-init src)]
                                          [test (stmt:for-test src)]
                                          [update (stmt:for-update src)])
                                      (quasiquote
                                       (for- (unquote
                                              (append
                                               (if init
                                                   (list (convert init) ':)
                                                   (list ':))
                                               (list (convert test))
                                               (if update
                                                   (list ': (convert update))
                                                   (list ':))))
                                             (unquote (convert (stmt:for-body src))))))]
                   [(stmt:return? src) (let ([res (stmt:return-result src)])
                                         (if res
                                             (convert res)
                                             0))])]
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
                                      (unquote (list*
                                                (convert (expr:call-function src))
                                                (for/list
                                                    ([arg (expr:call-arguments src)])
                                                  (convert arg)))))]
                   [(expr:postfix? src) (list
                                         (convert (expr:postfix-op src))
                                         (convert (expr:postfix-expr src)))])]
    [(decl? src) (cond [(decl:vars? src) (let ([decls (decl:vars-declarators src)])
                                           (list*
                                            'begin
                                            (for/list ([decl decls])
                                              (let ([init (decl:declarator-initializer decl)]
                                                    [type (decl:declarator-type decl)])
                                                (if type
                                                    (if init
                                                        (quasiquote
                                                         (:= (unquote (convert (decl:vars-type src)))
                                                             [(unquote (convert (decl:declarator-id decl)))
                                                              (unquote (convert type))]
                                                             (unquote (convert (init:expr-expr init)))))
                                                        (quasiquote
                                                         (: (unquote (convert (decl:vars-type src)))
                                                            [(unquote (convert (decl:declarator-id decl)))
                                                             (unquote (convert type))])))
                                                    (if init
                                                        (quasiquote
                                                         (:= (unquote (convert (decl:vars-type src)))
                                                             (unquote (convert (decl:declarator-id decl)))
                                                             (unquote (convert (init:expr-expr init)))))
                                                        (quasiquote
                                                         (: (unquote (convert (decl:vars-type src)))
                                                            (unquote (convert (decl:declarator-id decl)))))))))))]
                       [(decl:function? src) (append
                                              (list 'define
                                                    (list*
                                                     (convert (decl:declarator-id (decl:function-declarator src)))
                                                     (for/list ([arg (convert (decl:declarator-type (decl:function-declarator src)))])
                                                       (convert arg))))
                                              (convert (decl:function-body src)))]
                       [(decl:formal? src) (convert (decl:declarator-id (decl:formal-declarator src)))])]
    ))

(for ([src (parse-program
            "void mat_set_init(int* Mat){
  int  i,j,k,l;
  float tt;
  for(i=0; i<mrows; i++)
    for(j=0; j<mcols; j++)
      for(k=0; k<mdeps; k++)
        MR(Mat,0,i,j,k)= i*i
          /(mrows - 1)*(mrows - 1);

}")])
  (pretty-print (convert src)))
(parse-program "void mat_set_init(int* Mat){
  int  i,j,k,l;
  float tt;
  for(i=0; i<Mat->mrows; i++)
    for(j=0; j<Mat->mcols; j++)
      for(k=0; k<Mat->mdeps; k++)
        MR(Mat,0,i,j,k)= (float)(i*i)
          /(float)((Mat->mrows - 1)*(Mat->mrows - 1));

}")