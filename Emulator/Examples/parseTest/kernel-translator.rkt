#lang racket

(require c c/parse)

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
                                        [(eq? (id:var-name src) 'cudaMemcpyDeviceToHost) (quote "cudaMemcpyDeviceToHost")]
                                        [(eq? (id:var-name src) 'cudaMemcpyHostToDevice) (quote "cudaMemcpyHosttoDevice")]
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
                   [(stmt:if? src) (if (not (stmt:if-alt src)) (quasiquote
                                                                (if-
                                                                 (unquote (kernel-translator (stmt:if-test src)))
                                                                 (unquote (kernel-translator (stmt:if-cons src)))))
                                       (quasiquote
                                        (if-
                                         (unquote (kernel-translator (stmt:if-test src)))
                                         (unquote (kernel-translator (stmt:if-cons src)))
                                         (unquote (kernel-translator (stmt:if-alt src))))))]
                   [(stmt:for? src) (let ([init (stmt:for-init src)]
                                          [test (stmt:for-test src)]
                                          [update (stmt:for-update src)])
                                      (quasiquote
                                       (for- (unquote
                                              (append
                                               (if init
                                                   (list (kernel-translator init) ':)
                                                   (list ':))
                                               (list (kernel-translator test))
                                               (if update
                                                   (list ': (kernel-translator update))
                                                   (list ':))))
                                             (unquote (kernel-translator (stmt:for-body src))))))]
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

(pretty-print
 (for/list ([src (parse-program
                  "void jacobi(float *a0, float *a1, float *a2, float *a3, float *b0, float *b1, float *b2, float *c0, float *c1, float *c2, float *p, float *wrk1, float *wrk2, float *bnd, int nn, int imax, int jmax, int kmax, float omega, float *gosa){
        static int imax, jmax, kmax;
        static float omega;
        int i, j, k, n;
	float s0, ss, temp;
	const int tid = threadIdx.x;
	const int size = (imax-1)/(imax-1);

	for(n=0;n<nn;++n){

	temp=0.0;

		for(i=tid*size ; i<(tid+1)*size ; ++i)
			for(j=1 ; j<jmax-1 ; ++j)
				for(k=1 ; k<kmax-1 ; ++k){
					s0 = a0[i*jmax*kmax+j*kmax+k] * p[(i+1)*jmax*kmax+j*kmax+k]
					   + a1[i*jmax*kmax+j*kmax+k] * p[i*jmax*kmax+(j+1)*kmax+k]
					   + a2[i*jmax*kmax+j*kmax+k] * p[i*jmax*kmax+j*kmax+(k+1)]
					   + b0[i*jmax*kmax+j*kmax+k] * ( p[(i+1)*jmax*kmax+(j+1)*kmax+k] - p[(i+1)*jmax*kmax+(j-1)*kmax+k]
						       - p[(i-1)*jmax*kmax+(j+1)*kmax+k] + p[(i-1)*jmax*kmax+(j-1)*kmax+k] )
					   + b1[i*jmax*kmax+j*kmax+k] * ( p[i*jmax*kmax+(j+1)*kmax+(k+1)] - p[i*jmax*kmax+(j-1)*kmax+(k+1)]
						       - p[i*jmax*kmax+(j+1)*kmax+(k-1)] + p[i*jmax*kmax+(j-1)*kmax+(k-1)] )
					   + b2[i*jmax*kmax+j*kmax+k] * ( p[(i+1)*jmax*kmax+j*kmax+(k+1)] - p[(i-1)*jmax*kmax+j*kmax+(k+1)]
						       - p[(i+1)*jmax*kmax+j*kmax+(k-1)] + p[(i-1)*jmax*kmax+j*kmax+(k-1)] )
					   + c0[i*jmax*kmax+j*kmax+k] * p[(i-1)*jmax*kmax+j*kmax+k]
					   + c1[i*jmax*kmax+j*kmax+k] * p[i*jmax*kmax+(j-1)*kmax+k]
					   + c2[i*jmax*kmax+j*kmax+k] * p[i*jmax*kmax+j*kmax+(k-1)]
					   + wrk1[i*jmax*kmax+j*kmax+k];

					ss = ( s0 * a3[i*jmax*kmax+j*kmax+k] - p[i*jmax*kmax+j*kmax+k] ) * bnd[i*jmax*kmax+j*kmax+k];

					temp = temp + ss*ss;

					wrk2[i*jmax*kmax+j*kmax+k] = p[i*jmax*kmax+j*kmax+k] + omega * ss;
		}

	for(i=tid*size ; i<(tid+1)*size ; ++i)
		for(j=1 ; j<jmax-1 ; ++j)
			for(k=1 ; k<kmax-1 ; ++k)
				p[i*jmax*kmax+j*kmax+k] = wrk2[i*jmax*kmax+j*kmax+k];
	}

	gosa[tid] = temp;
}
")])
   (kernel-translator src)))
(parse-program "int main(){
cudaMalloc((void**)&dev_wrk2, N_IJK*sizeof(float));
float i = 0.0;
cudaMemcpy(dev_p, p, N_IJK*sizeof(float), cudaMemcpyHostToDevice);}")