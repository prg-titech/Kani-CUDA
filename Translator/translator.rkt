#lang rosette

(require "host-translator.rkt"
         "kernel-translator.rkt"
         control
         c)

(define device-name-space '())

(define (desired-line? line)
  (not
   (or (string-contains? line "\"")
       (string-contains? line "#"))))

(define (convert-line line)
  (cond
    [(string-contains? line "__global__") (begin
                                            (set! device-name-space (list* (list-ref (string-split line #rx"[ |(]") 2) device-name-space))
                                            (string-replace line "__global__" ""))]
    ;; TODO : diffusion2d_temporal_blocking.cuでおかしなことが起こってる
    [(string-contains? line "__shared__") (let ([name (list-ref (string-split line #rx"[ |[]") 2)])
                                            ;(println (string-split line #rx"[ |[]"))
                                            (string-replace (string-replace line "__shared__" "") name (string-append "__shared__" name)))]
    [(string-contains? line "dim3 ") (string-replace line "dim3 " "")]
    [(and (string-contains? line "<<<") (string-contains? line ">>>(")) (string-replace (string-replace line "<<<" "(") ">>>(" ",")]
    [(desired-line? line) line]
    [else ""]))

(define (cpp file)
  (define in (open-input-file file))
  (define cp (open-output-file "__cp.cu" #:exists 'truncate))
  (define dst (open-output-file "__cp.cu" #:exists 'truncate))
  (define line (read-line in 'any))
  (when (not (eof-object? line))
    (set! line (string-append line "\n")))
  (while (not (eof-object? line)) 
         (when (not (string-contains? line "#include"))
           (fprintf cp line))
         (set! line (read-line in 'any))
         (when (not (eof-object? line))
           (set! line (string-append line "\n"))))
  (close-input-port in)
  (close-output-port cp)
  (close-output-port dst)
  (system* "/usr/bin/cpp" "__cp.cu" "__dst.cu"))
  

(define (translate file)
  (cpp file)
  (define in (open-input-file "__dst.cu"))
  (define line (read-line in))
  (when (not (eof-object? line))
    (set! line (string-append line "\n")))
  (define program '())
  (while (not (eof-object? line))
         (set! program (list program (convert-line line)))
         (set! line (read-line in))
         (when (not (eof-object? line))
           (set! line (string-append line "\n"))))
  (close-input-port in)
  ;(pretty-print (string-join (flatten program)))
  (for ([src (parse-program (string-join (flatten program)))])
    (if (member (symbol->string (get-name src)) device-name-space)
        (pretty-display (kernel-translator src))
        (pretty-display (host-translator src)))))

;(translate "/Users/akira/masuhara-lab/Kani-CUDA/kani-cudaBMT/himeno_temporal_blocking.cu")
(translate "/Users/akira/masuhara-lab/Kani-CUDA/Emulator/Examples/Diffusion2d/diffusion2d_temporal_blocking.cu")

;(pretty-print (kernel-translator (list-ref (parse-program "void jacobi(float *a0, float *a1, float *a2, float *a3, float *b0, float *b1, float *b2, float *c0, float *c1, float *c2, float *p, float *wrk1, float *wrk2, float *bnd, int nn, int imax, int jmax, int kmax, float omega, float *gosa){
;	int i, j, k, j2, k2, n, xy, c, csb;
;	float s0, ss, temp;
;	//const int size = (imax-1)/(imax-1);
;	k = threadIdx.x + (blockDim.x-2) * blockIdx.x + 1;
;	j = threadIdx.y + (blockDim.y-2) * blockIdx.y + 1;
;	k2 = threadIdx.x + blockDim.x * blockIdx.x;
;	j2 = threadIdx.y + blockDim.y * blockIdx.y;
;	const int tid = (k-1) + (j-1) * (kmax-2);
;	xy = kmax * jmax;
;	float sb[BLOCKSIZE];
;	csb = threadIdx.x + threadIdx.y * blockDim.x;
;	for(n=0;n<nn;++n){
;		c = j * kmax + k;
;		temp=0.0;
;		if(0 < threadIdx.x && k < kmax-1 && 0 < j && j < jmax-1){
;			for(i=1 ; i<imax-1 ; ++i){
;				syncthreads();
;				s0 = a0[i*jmax*kmax+j*kmax+k] * p[(i+1)*jmax*kmax+j*kmax+k]
;				+ a1[i*jmax*kmax+j*kmax+k] * p[i*jmax*kmax+(j+1)*kmax+k]
;				+ a2[i*jmax*kmax+j*kmax+k] * p[i*jmax*kmax+j*kmax+(k+1)]
;				+ b0[i*jmax*kmax+j*kmax+k] 
;					*(p[(i+1)*jmax*kmax+(j+1)*kmax+k] 
;					- p[(i+1)*jmax*kmax+(j-1)*kmax+k]
;					- p[(i-1)*jmax*kmax+(j+1)*kmax+k] 
;					+ p[(i-1)*jmax*kmax+(j-1)*kmax+k] )
;				+ b1[i*jmax*kmax+j*kmax+k] 
;					*(p[i*jmax*kmax+(j+1)*kmax+(k+1)] 
;					- p[i*jmax*kmax+(j-1)*kmax+(k+1)]
;					- p[i*jmax*kmax+(j-1)*kmax+(k-1)]
;					+ p[i*jmax*kmax+(j+1)*kmax+(k-1)])
;				+ b2[i*jmax*kmax+j*kmax+k] 
;					*(p[(i+1)*jmax*kmax+j*kmax+(k+1)] 
;					- p[(i-1)*jmax*kmax+j*kmax+(k+1)]
;					- p[(i+1)*jmax*kmax+j*kmax+(k-1)] 
;					+ p[(i-1)*jmax*kmax+j*kmax+(k-1)] )
;				+ c0[i*jmax*kmax+j*kmax+k] * p[(i-1)*jmax*kmax+j*kmax+k]
;				+ c1[i*jmax*kmax+j*kmax+k] * p[i*jmax*kmax+(j-1)*kmax+k]
;				+ c2[i*jmax*kmax+j*kmax+k] * p[i*jmax*kmax+j*kmax+(k-1)]
;				+ wrk1[i*jmax*kmax+j*kmax+k];
;
;				ss = ( s0 * a3[i*jmax*kmax+j*kmax+k] - p[i*jmax*kmax+j*kmax+k] ) * bnd[i*jmax*kmax+j*kmax+k];
;
;				temp = temp + ss*ss;
;
;				wrk2[i*THREAD_NUM+j2*BLOCKSIZEX*GRIDSIZEX+k2] = p[i*jmax*kmax+j*kmax+k] + omega * ss;
;				c += xy;
;			}
;		}
;		syncthreads();
;		if(0 < threadIdx.x && threadIdx.x < blockDim.x-1 && 0 < threadIdx.y && threadIdx.y < blockDim.y-1){
;			for(i=1; i<imax-1; i++){
;				s0 = a0[i*jmax*kmax+j*kmax+k] * wrk2[(i+1)*THREAD_NUM+j2*BLOCKSIZEX*GRIDSIZEX+k2]
;				+ a1[i*jmax*kmax+j*kmax+k] * wrk2[i*THREAD_NUM+(j2+1)*BLOCKSIZEX*GRIDSIZEX+k2]
;				+ a2[i*jmax*kmax+j*kmax+k] * wrk2[i*THREAD_NUM+j2*BLOCKSIZEX*GRIDSIZEX+(k2+1)]
;				+ b0[i*jmax*kmax+j*kmax+k] 
;					*(wrk2[(i+1)*THREAD_NUM+(j2+1)*BLOCKSIZEX*GRIDSIZEX+k2] 
;					- wrk2[(i+1)*THREAD_NUM+(j2-1)*BLOCKSIZEX*GRIDSIZEX+k2]
;					- wrk2[(i-1)*THREAD_NUM+(j2+1)*BLOCKSIZEX*GRIDSIZEX+k2] 
;					+ wrk2[(i-1)*THREAD_NUM+(j2-1)*BLOCKSIZEX*GRIDSIZEX+k2] )
;				+ b1[i*jmax*kmax+j*kmax+k] 
;					*(wrk2[i*THREAD_NUM+(j2+1)*BLOCKSIZEX*GRIDSIZEX+(k2+1)] 
;					- wrk2[i*THREAD_NUM+(j2-1)*BLOCKSIZEX*GRIDSIZEX+(k2+1)]
;					- wrk2[i*THREAD_NUM+(j2-1)*BLOCKSIZEX*GRIDSIZEX+(k2-1)]
;					+ wrk2[i*THREAD_NUM+(j2+1)*BLOCKSIZEX*GRIDSIZEX+(k2-1)])
;				+ b2[i*jmax*kmax+j*kmax+k] 
;					*(wrk2[(i+1)*THREAD_NUM+j2*BLOCKSIZEX*GRIDSIZEX+(k2+1)] 
;					- wrk2[(i-1)*THREAD_NUM+j2*BLOCKSIZEX*GRIDSIZEX+(k2+1)]
;					- wrk2[(i+1)*THREAD_NUM+j2*BLOCKSIZEX*GRIDSIZEX+(k2-1)] 
;					+ wrk2[(i-1)*THREAD_NUM+j2*BLOCKSIZEX*GRIDSIZEX+(k2-1)] )
;				+ c0[i*jmax*kmax+j*kmax+k] * wrk2[(i-1)*THREAD_NUM+j2*BLOCKSIZEX*GRIDSIZEX+k2]
;				+ c1[i*jmax*kmax+j*kmax+k] * wrk2[i*THREAD_NUM+(j2-1)*BLOCKSIZEX*GRIDSIZEX+k2]
;				+ c2[i*jmax*kmax+j*kmax+k] * wrk2[i*THREAD_NUM+j2*BLOCKSIZEX*GRIDSIZEX+(k2-1)]
;				+ wrk1[i*jmax*kmax+j*kmax+k];
;
;				ss = ( s0 * a3[i*jmax*kmax+j*kmax+k] - wrk2[i*THREAD_NUM+j2*BLOCKSIZEX*GRIDSIZEX+k2] ) * bnd[i*jmax*kmax+j*kmax+k];
;
;				temp = temp + ss*ss;
;
;				p[i*jmax*kmax+j*kmax+k] = wrk2[i*THREAD_NUM+j2*BLOCKSIZEX*GRIDSIZEX+k2] + omega * ss;
;				c += xy;
;			}
;		}	
;	} /* end n loop */
;	syncthreads();
;	gosa[tid] = temp;
;}") 0)))
;
