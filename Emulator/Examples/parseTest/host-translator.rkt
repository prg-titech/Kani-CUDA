#lang racket

(require c c/parse)

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
                                        [(eq? (id:var-name src) 'cudaMemcpyDeviceToHost) (quote "cudaMemcpyDeviceToHost")]
                                        [(eq? (id:var-name src) 'cudaMemcpyHostToDevice) (quote "cudaMemcpyHosttoDevice")]
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
                                      (quasiquote
                                       (for- (unquote
                                              (append
                                               (if init
                                                   (list (host-translator init) ':)
                                                   (list ':))
                                               (list (host-translator test))
                                               (if update
                                                   (list ': (host-translator update))
                                                   (list ':))))
                                             (unquote (host-translator (stmt:for-body src))))))]
                   [(stmt:return? src) (let ([res (stmt:return-result src)])
                                         (if res
                                             (host-translator res)
                                             0))])]
    [(expr? src) (cond
                   [(expr:int? src) (expr:int-value src)]
                   [(expr:float? src) (expr:float-value src)]
                   [(expr:unop? src) (quasiquote
                                      ((unquote (host-translator (expr:unop-op src)))
                                       (unquote (host-translator (expr:unop-expr src)))))]
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
                   [(expr:call? src) (list*
                                      (host-translator (expr:call-function src))
                                      (for/list
                                          ([arg (expr:call-arguments src)])
                                        (host-translator arg)))]
                   [(expr:postfix? src) (list
                                         (host-translator (expr:postfix-op src))
                                         (host-translator (expr:postfix-expr src)))]
                   [(expr:prefix? src) (list
                                        (host-translator (expr:prefix-op src))
                                        (host-translator (expr:prefix-expr src)))]
                   [(expr:sizeof? src) '1]
                   [(expr:cast? src) (host-translator (expr:cast-expr src))])]
    [(decl? src) (cond [(decl:vars? src) (let ([decls (decl:vars-declarators src)])
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
                                              (list 'define
                                                    (list*
                                                     (host-translator (decl:declarator-id (decl:function-declarator src)))
                                                     (for/list ([arg (host-translator (decl:declarator-type (decl:function-declarator src)))])
                                                       (host-translator arg))))
                                              (host-translator (decl:function-body src)))]
                       [(decl:formal? src) (host-translator (decl:declarator-id (decl:formal-declarator src)))])]
    ))

(pretty-print
 (for/list ([src (parse-program
                  "int main(){
        static int imax, jmax, kmax;
        static float omega;
	int i, j, k;
	float final_gosa;
	double cpu0, cpu1, nflop, xmflops2, score;

	float gosa[127];


	float *p;
	float *a0, *a1, *a2, *a3;
	float *b0, *b1, *b2;
	float *c0, *c1, *c2;

	float *bnd;
	float *wrk1, *wrk2;

	imax = 129 -1;
	jmax = 65 -1;
	kmax = 65 -1;
	//int N_IJK = 129*65*65;
	int N_IJK = imax*jmax*kmax;

	float *dev_p;
	float *dev_a0, *dev_a1, *dev_a2, *dev_a3;
	float *dev_b0, *dev_b1, *dev_b2;
	float *dev_c0, *dev_c1, *dev_c2;

	float *dev_bnd;
	float *dev_wrk1, *dev_wrk2;

	float *dev_gosa;



	omega = 0.8;

	//initial_maxtrix();


	a0 = (float*)malloc(sizeof(float)*N_IJK);
	a1 = (float*)malloc(sizeof(float)*N_IJK);
	a2 = (float*)malloc(sizeof(float)*N_IJK);
	a3 = (float*)malloc(sizeof(float)*N_IJK);

	b0 = (float*)malloc(sizeof(float)*N_IJK);
	b1 = (float*)malloc(sizeof(float)*N_IJK);
	b2 = (float*)malloc(sizeof(float)*N_IJK);

	c0 = (float*)malloc(sizeof(float)*N_IJK);
	c1 = (float*)malloc(sizeof(float)*N_IJK);
	c2 = (float*)malloc(sizeof(float)*N_IJK);

	p = (float*)malloc(sizeof(float)*N_IJK);

	wrk1 = (float*)malloc(sizeof(float)*N_IJK);
	wrk2 = (float*)malloc(sizeof(float)*N_IJK);
	bnd = (float*)malloc(sizeof(float)*N_IJK);

	//gosa = (float*)malloc(sizeof(float));



	cudaMalloc((void**)&dev_a0, N_IJK*sizeof(float));
	cudaMalloc((void**)&dev_a1, N_IJK*sizeof(float));
	cudaMalloc((void**)&dev_a2, N_IJK*sizeof(float));
	cudaMalloc((void**)&dev_a3, N_IJK*sizeof(float));

	cudaMalloc((void**)&dev_b0, N_IJK*sizeof(float));
	cudaMalloc((void**)&dev_b1, N_IJK*sizeof(float));
	cudaMalloc((void**)&dev_b2, N_IJK*sizeof(float));

	cudaMalloc((void**)&dev_c0, N_IJK*sizeof(float));
	cudaMalloc((void**)&dev_c1, N_IJK*sizeof(float));
	cudaMalloc((void**)&dev_c2, N_IJK*sizeof(float));

	cudaMalloc((void**)&dev_p, N_IJK*sizeof(float));

	cudaMalloc((void**)&dev_bnd, N_IJK*sizeof(float));
	cudaMalloc((void**)&dev_wrk1, N_IJK*sizeof(float));
	cudaMalloc((void**)&dev_wrk2, N_IJK*sizeof(float));

	cudaMalloc((void**)&dev_gosa, sizeof(float)*127);

	//int i,j,k;

	for(i=0 ; i<imax ; ++i)
		for(j=0 ; j<jmax ; ++j)
			for(k=0 ; k<kmax ; ++k){
				a0[i*jmax*kmax+j*kmax+k]=0.0;
				a1[i*jmax*kmax+j*kmax+k]=0.0;
				a2[i*jmax*kmax+j*kmax+k]=0.0;
				a3[i*jmax*kmax+j*kmax+k]=0.0;
				b0[i*jmax*kmax+j*kmax+k]=0.0;
				b1[i*jmax*kmax+j*kmax+k]=0.0;
				b2[i*jmax*kmax+j*kmax+k]=0.0;
				c0[i*jmax*kmax+j*kmax+k]=0.0;
				c1[i*jmax*kmax+j*kmax+k]=0.0;
				c2[i*jmax*kmax+j*kmax+k]=0.0;
				p[i*jmax*kmax+j*kmax+k]=0.0;
				wrk1[i*jmax*kmax+j*kmax+k]=0.0;
				bnd[i*jmax*kmax+j*kmax+k]=0.0;
	      		}

	for(i=0 ; i<imax ; ++i)
		for(j=0 ; j<jmax ; ++j)
			for(k=0 ; k<kmax ; ++k){
				a0[i*jmax*kmax+j*kmax+k]=1.0;
				a1[i*jmax*kmax+j*kmax+k]=1.0;
				a2[i*jmax*kmax+j*kmax+k]=1.0;
				a3[i*jmax*kmax+j*kmax+k]=1.0/6.0;
				b0[i*jmax*kmax+j*kmax+k]=0.0;
				b1[i*jmax*kmax+j*kmax+k]=0.0;
				b2[i*jmax*kmax+j*kmax+k]=0.0;
				c0[i*jmax*kmax+j*kmax+k]=1.0;
				c1[i*jmax*kmax+j*kmax+k]=1.0;
				c2[i*jmax*kmax+j*kmax+k]=1.0;
				p[i*jmax*kmax+j*kmax+k]=(float)(k*k)/(float)((kmax-1)*(kmax-1));
				wrk1[i*jmax*kmax+j*kmax+k]=0.0;
				bnd[i*jmax*kmax+j*kmax+k]=1.0;
			}

	cudaMemcpy(dev_a0, a0, N_IJK*sizeof(float), cudaMemcpyHostToDevice);
	cudaMemcpy(dev_a1, a1, N_IJK*sizeof(float), cudaMemcpyHostToDevice);
	cudaMemcpy(dev_a2, a2, N_IJK*sizeof(float), cudaMemcpyHostToDevice);
	cudaMemcpy(dev_a3, a3, N_IJK*sizeof(float), cudaMemcpyHostToDevice);
	cudaMemcpy(dev_b0, b0, N_IJK*sizeof(float), cudaMemcpyHostToDevice);
	cudaMemcpy(dev_b1, b1, N_IJK*sizeof(float), cudaMemcpyHostToDevice);
	cudaMemcpy(dev_b2, b2, N_IJK*sizeof(float), cudaMemcpyHostToDevice);
	cudaMemcpy(dev_c0, c0, N_IJK*sizeof(float), cudaMemcpyHostToDevice);
	cudaMemcpy(dev_c1, c1, N_IJK*sizeof(float), cudaMemcpyHostToDevice);
	cudaMemcpy(dev_c2, c2, N_IJK*sizeof(float), cudaMemcpyHostToDevice);
	cudaMemcpy(dev_wrk1, wrk1, N_IJK*sizeof(float), cudaMemcpyHostToDevice);
	cudaMemcpy(dev_wrk2, wrk2, N_IJK*sizeof(float), cudaMemcpyHostToDevice);
	cudaMemcpy(dev_bnd, bnd, N_IJK*sizeof(float), cudaMemcpyHostToDevice);
	cudaMemcpy(dev_p, p, N_IJK*sizeof(float), cudaMemcpyHostToDevice);

	//cudaMemcpy(dev_gosa, gosa, sizeof(float), cudaMemcpyHostToDevice);


	cpu0 = second();

	jacobi(1,127, dev_a0, dev_a1, dev_a2, dev_a3, dev_b0, dev_b1, dev_b2, dev_c0, dev_c1, dev_c2, dev_p, dev_wrk1, dev_wrk2, dev_bnd, 200, imax, jmax, kmax, omega, dev_gosa);

	//cudaDeviceSynchronize();

	cpu1 = second();

	cudaMemcpy(&gosa, dev_gosa, sizeof(float)*127, cudaMemcpyDeviceToHost);


	cudaFree(dev_a0);
	cudaFree(dev_a1);
	cudaFree(dev_a2);
	cudaFree(dev_a3);
	cudaFree(dev_b0);
	cudaFree(dev_b1);
	cudaFree(dev_b2);
	cudaFree(dev_c0);
	cudaFree(dev_c1);
	cudaFree(dev_c2);
	cudaFree(dev_p);
	cudaFree(dev_wrk1);
	cudaFree(dev_wrk2);
	cudaFree(dev_bnd);

	cudaFree(dev_gosa);



	for(int gosa_index=0; gosa_index<127; gosa_index++){
		final_gosa += gosa[gosa_index];
	}


	nflop = (kmax-2)*(jmax-2)*(imax-2)*34;

	//if(cpu1 != 0.0)
	//	xmflops2 = nflop/cpu1*1.0e-6*(float)200;

	//score = xmflops2/32.27;

	return(0);

}")])
   (host-translator src)))
;(parse-program "int main(){
;cudaMalloc((void**)&dev_wrk2, N_IJK*sizeof(float));
;float i = 0.0;
;a[1] = 0;
;cudaMemcpy(dev_p, p, N_IJK*sizeof(float), cudaMemcpyHostToDevice);}")