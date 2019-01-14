# 1 "__cp.cu"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 330 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "__cp.cu" 2
# 15 "__cp.cu"
























static int imax, jmax, kmax, mimax, mjmax, mkmax;
static float omega;

double second(){
	struct timeval tm;
	double t;

	static int base_sec = 0, base_usec = 0;

	gettimeofday(&tm, NULL);

	if(base_sec == 0 && base_usec == 0){
		base_sec = tm.tv_sec;
		base_usec = tm.tv_usec;
		t = 0.0;
	}
	else{
		t = (double)(tm.tv_sec-base_sec) + ((double)(tm.tv_usec-base_usec))/1.0e6;
	}

	return t;
}


__global__ void jacobi(float *a0, float *a1, float *a2, float *a3, float *b0, float *b1, float *b2, float *c0, float *c1, float *c2, float *p, float *wrk1, float *wrk2, float *bnd, int nn, int imax, int jmax, int kmax, float omega, float *gosa){
	int i, j, k, j2, k2, n, xy, c, csb;
	float s0, ss, temp;
	//const int size = (imax-1)/(imax-1);
	k = threadIdx.x + (blockDim.x-2) * blockIdx.x + 1;
	j = threadIdx.y + (blockDim.y-2) * blockIdx.y + 1;
	k2 = threadIdx.x + blockDim.x * blockIdx.x;
	j2 = threadIdx.y + blockDim.y * blockIdx.y;
	const int tid = (k-1) + (j-1) * (kmax-2);
	xy = kmax * jmax;
	__shared__ float sb[6 * 4];
	csb = threadIdx.x + threadIdx.y * blockDim.x;
	for(n=0;n<nn;++n){
		c = j * kmax + k;
		temp=0.0;
		for(i=1 ; i<imax-1 ; ++i){
				syncthreads();
			if(0 < threadIdx.x && k < kmax-1 && 0 < j && j < jmax-1){
				s0 = a0[i*jmax*kmax+j*kmax+k] * p[(i+1)*jmax*kmax+j*kmax+k]
				+ a1[i*jmax*kmax+j*kmax+k] * p[i*jmax*kmax+(j+1)*kmax+k]
				+ a2[i*jmax*kmax+j*kmax+k] * p[i*jmax*kmax+j*kmax+(k+1)]
				+ b0[i*jmax*kmax+j*kmax+k] 
					*(p[(i+1)*jmax*kmax+(j+1)*kmax+k] 
					- p[(i+1)*jmax*kmax+(j-1)*kmax+k]
					- p[(i-1)*jmax*kmax+(j+1)*kmax+k] 
					+ p[(i-1)*jmax*kmax+(j-1)*kmax+k] )
				+ b1[i*jmax*kmax+j*kmax+k] 
					*(p[i*jmax*kmax+(j+1)*kmax+(k+1)] 
					- p[i*jmax*kmax+(j-1)*kmax+(k+1)]
					- p[i*jmax*kmax+(j-1)*kmax+(k-1)]
					+ p[i*jmax*kmax+(j+1)*kmax+(k-1)])
				+ b2[i*jmax*kmax+j*kmax+k] 
					*(p[(i+1)*jmax*kmax+j*kmax+(k+1)] 
					- p[(i-1)*jmax*kmax+j*kmax+(k+1)]
					- p[(i+1)*jmax*kmax+j*kmax+(k-1)] 
					+ p[(i-1)*jmax*kmax+j*kmax+(k-1)] )
				+ c0[i*jmax*kmax+j*kmax+k] * p[(i-1)*jmax*kmax+j*kmax+k]
				+ c1[i*jmax*kmax+j*kmax+k] * p[i*jmax*kmax+(j-1)*kmax+k]
				+ c2[i*jmax*kmax+j*kmax+k] * p[i*jmax*kmax+j*kmax+(k-1)]
				+ wrk1[i*jmax*kmax+j*kmax+k];

				ss = ( s0 * a3[i*jmax*kmax+j*kmax+k] - p[i*jmax*kmax+j*kmax+k] ) * bnd[i*jmax*kmax+j*kmax+k];

				temp = temp + ss*ss;

				wrk2[i*6 * 4 * 3 * 6 +j2*6*3 +k2] = p[i*jmax*kmax+j*kmax+k] + omega * ss;
				c += xy;
			}
		}
		syncthreads();
		if(0 < threadIdx.x && threadIdx.x < blockDim.x-1 && 0 < threadIdx.y && threadIdx.y < blockDim.y-1){
			for(i=1; i<imax-1; i++){
				s0 = a0[i*jmax*kmax+j*kmax+k] * wrk2[(i+1)*6 * 4 * 3 * 6 +j2*6*3 +k2]
				+ a1[i*jmax*kmax+j*kmax+k] * wrk2[i*6 * 4 * 3 * 6 +(j2+1)*6*3 +k2]
				+ a2[i*jmax*kmax+j*kmax+k] * wrk2[i*6 * 4 * 3 * 6 +j2*6*3 +(k2+1)]
				+ b0[i*jmax*kmax+j*kmax+k] 
					*(wrk2[(i+1)*6 * 4 * 3 * 6 +(j2+1)*6*3 +k2] 
					- wrk2[(i+1)*6 * 4 * 3 * 6 +(j2-1)*6*3 +k2]
					- wrk2[(i-1)*6 * 4 * 3 * 6 +(j2+1)*6*3 +k2] 
					+ wrk2[(i-1)*6 * 4 * 3 * 6 +(j2-1)*6*3 +k2] )
				+ b1[i*jmax*kmax+j*kmax+k] 
					*(wrk2[i*6 * 4 * 3 * 6 +(j2+1)*6*3 +(k2+1)] 
					- wrk2[i*6 * 4 * 3 * 6 +(j2-1)*6*3 +(k2+1)]
					- wrk2[i*6 * 4 * 3 * 6 +(j2-1)*6*3 +(k2-1)]
					+ wrk2[i*6 * 4 * 3 * 6 +(j2+1)*6*3 +(k2-1)])
				+ b2[i*jmax*kmax+j*kmax+k] 
					*(wrk2[(i+1)*6 * 4 * 3 * 6 +j2*6*3 +(k2+1)] 
					- wrk2[(i-1)*6 * 4 * 3 * 6 +j2*6*3 +(k2+1)]
					- wrk2[(i+1)*6 * 4 * 3 * 6 +j2*6*3 +(k2-1)] 
					+ wrk2[(i-1)*6 * 4 * 3 * 6 +j2*6*3 +(k2-1)] )
				+ c0[i*jmax*kmax+j*kmax+k] * wrk2[(i-1)*6 * 4 * 3 * 6 +j2*6*3 +k2]
				+ c1[i*jmax*kmax+j*kmax+k] * wrk2[i*6 * 4 * 3 * 6 +(j2-1)*6*3 +k2]
				+ c2[i*jmax*kmax+j*kmax+k] * wrk2[i*6 * 4 * 3 * 6 +j2*6*3 +(k2-1)]
				+ wrk1[i*jmax*kmax+j*kmax+k];

				ss = ( s0 * a3[i*jmax*kmax+j*kmax+k] - wrk2[i*6 * 4 * 3 * 6 +j2*6*3 +k2] ) * bnd[i*jmax*kmax+j*kmax+k];

				temp = temp + ss*ss;

				p[i*jmax*kmax+j*kmax+k] = wrk2[i*6 * 4 * 3 * 6 +j2*6*3 +k2] + omega * ss;
				c += xy;
			}
		}	
	} 
	syncthreads();
	gosa[tid] = temp;
}

int main(){
	int i, j, k;
	float final_gosa;
	double cpu0, cpu1, nflop, xmflops2, score;

	float gosa[6 * 4 * 3 * 6];

	
	float *p;
	float *a0, *a1, *a2, *a3;
	float *b0, *b1, *b2;
	float *c0, *c1, *c2;

	float *bnd;
	float *wrk1, *wrk2;
	
	mimax = 4;
	mjmax = 6 * (4 - 2) + 2;
	mkmax = 3 * (6 - 2) + 2;
	imax = 4 -1;
	jmax = 6 * (4 - 2) + 2 -1;
	kmax = 3 * (6 - 2) + 2 -1;
	//int N_IJK = 4*6 * (4 - 2) + 2*3 * (6 - 2) + 2;
	int N_IJK = mimax*mjmax*mkmax;
	int WORKSIZE = 6 * 4 * 3 * 6*mimax;
	
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
	wrk2 = (float*)malloc(sizeof(float)*WORKSIZE);
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
	cudaMalloc((void**)&dev_wrk2, WORKSIZE*sizeof(float));

	cudaMalloc((void**)&dev_gosa, sizeof(float)*6 * 4 * 3 * 6);
	

	
	//int i,j,k;
	
# 259 "__cp.cu"

	for(i=0 ; i<mimax ; ++i){
		for(j=0 ; j<mjmax ; ++j){
			for(k=0 ; k<mkmax ; ++k){
				a0[i*mjmax*mkmax+j*mkmax+k]=1.0;
				a1[i*mjmax*mkmax+j*mkmax+k]=1.0;
				a2[i*mjmax*mkmax+j*mkmax+k]=1.0;
				a3[i*mjmax*mkmax+j*mkmax+k]=1.0/6.0;
				b0[i*mjmax*mkmax+j*mkmax+k]=0.0;
				b1[i*mjmax*mkmax+j*mkmax+k]=0.0;
				b2[i*mjmax*mkmax+j*mkmax+k]=0.0;
				c0[i*mjmax*mkmax+j*mkmax+k]=1.0;
				c1[i*mjmax*mkmax+j*mkmax+k]=1.0;
				c2[i*mjmax*mkmax+j*mkmax+k]=1.0;
				p[i*mjmax*mkmax+j*mkmax+k]=(float)(i*i)/(float)(imax*imax);
				wrk1[i*mjmax*mkmax+j*mkmax+k]=0.0;
				bnd[i*mjmax*mkmax+j*mkmax+k]=1.0;
			}
		}
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
	cudaMemcpy(dev_wrk2, wrk2, WORKSIZE*sizeof(float), cudaMemcpyHostToDevice);
	cudaMemcpy(dev_bnd, bnd, N_IJK*sizeof(float), cudaMemcpyHostToDevice);
	cudaMemcpy(dev_p, p, N_IJK*sizeof(float), cudaMemcpyHostToDevice);

	//cudaMemcpy(dev_gosa, gosa, sizeof(float), cudaMemcpyHostToDevice);
	

	printf("mimax = %d mjmax = %d mkmax = %d\n", 4, 6 * (4 - 2) + 2, 3 * (6 - 2) + 2);
	printf("imax = %d jmax = %d kmax = %d\n", imax, jmax, kmax);

	cpu0 = second(); 

	dim3 block(6, 4, 1);
	dim3 grid(3, 6, 1);

	jacobi<<<grid, block>>>(dev_a0, dev_a1, dev_a2, dev_a3, dev_b0, dev_b1, dev_b2, dev_c0, dev_c1, dev_c2, dev_p, dev_wrk1, dev_wrk2, dev_bnd, 3, mimax, mjmax, mkmax, omega, dev_gosa);

	cudaDeviceSynchronize();

	cpu1 = second();

	cudaMemcpy(&gosa, dev_gosa, sizeof(float)*6 * 4 * 3 * 6, cudaMemcpyDeviceToHost);

	
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
	

	
	for(int gosa_index=0; gosa_index<6 * 4 * 3 * 6; gosa_index++){
		//printf("%f\n", gosa[gosa_index]);
		final_gosa += gosa[gosa_index];
		//printf("Gosa%d: %e \n", gosa_index, gosa[gosa_index]);
	}
	

	nflop = (kmax-2)*(jmax-2)*(imax-2)*34;

	if(cpu1 != 0.0){
		xmflops2 = nflop/cpu1*1.0e-6*(float)3;
	}

	score = xmflops2/32.27;

	printf("gpu: %f sec.\n", cpu1);
	printf("Loop executed for %d times\n", 3);
	printf("Gosa: %e \n", final_gosa);
	//printf("MFLOPS measured: %f\n", xmflops2);
	//printf("Score: %f\n", score);

	return(0);
}

