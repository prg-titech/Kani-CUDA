#include<stdio.h>
#include<sys/time.h>

#define BLOCKSIZEX 64
#define BLOCKSIZEY 8
#define BLOCKSIZE BLOCKSIZEX * BLOCKSIZEY
#define GRIDSIZEX 8
#define GRIDSIZEY 45
#define GRIDSIZE GRIDSIZEX * GRIDSIZEY
#define THREAD_NUM BLOCKSIZE * GRIDSIZE

#define MIMAX	256
#define MJMAX	GRIDSIZEY * (BLOCKSIZEY - 2) + 2
#define MKMAX	GRIDSIZEX * (BLOCKSIZEX - 2) + 2

#define NN 700

/*static float p[MIMAX][MJMAX][MKMAX];
static float a[MIMAX][MJMAX][MKMAX][4];
static float b[MIMAX][MJMAX][MKMAX][3];
static float c[MIMAX][MJMAX][MKMAX][3];
static float bnd[MIMAX][MJMAX][MKMAX];
static float work1[MIMAX][MJMAX][MKMAX];
static float work2[MIMAX][MJMAX][MKMAX];*/

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
	int i, j, k, j2, k2, n, xy, c, csb, csb2;
	float s0, ss, temp;
	//const int size = (imax-1)/(imax-1);
	k = threadIdx.x + (blockDim.x-2) * blockIdx.x;
	j = threadIdx.y + (blockDim.y-2) * blockIdx.y;
	k2 = threadIdx.x + blockDim.x * blockIdx.x;
	j2 = threadIdx.y + blockDim.y * blockIdx.y;
	const int tid = (k-1) + (j-1) * (kmax-2);
	xy = kmax * jmax;
	extern __shared__ float sb1[];
  extern __shared__ float sb2[];
  float *sb1_t, *sb1_m, *sb1_b, *sb2_t, *sb2_m, *sb2_b;
  sb1_t = sb1;
  sb1_m = sb1 + (blockDim.x + 2)*(blockDim.y + 2);
  sb1_b = sb1 + 2*(blockDim.x + 2)*(blockDim.y + 2);
  sb2_t = sb2;
  sb2_m = sb2 + BLOCKSIZE;
  sb2_b = sb2 + 2*BLOCKSIZE;
  csb = threadIdx.x + 1 + (threadIdx.y + 1) * (blockDim.x + 2);
	csb2 = threadIdx.x + threadIdx.y * blockDim.x;
  int nn_2 = (int)nn/2;
	for(n=0;n<nn_2;++n){
    i = 0;
		c = j * kmax + k;
		temp=0.0;
    sb1_t[csb] = p[c];
    sb1_m[csb] = p[c+xy];
    sb1_b[csb] = p[c+2*xy];
    if(threadIdx.x==0){
      sb1_t[csb-1] = p[(k==0) ? c : c - 1];
      sb1_m[csb-1] = p[(k==0) ? c+xy : c+xy - 1];
      sb1_b[csb-1] = p[(k==0) ? c+2*xy : c+2*xy - 1];
    }
    if(threadIdx.x==blockDim.x-1){
      sb1_t[csb+1] = p[(k==kmax-1) ? c : c + 1];
      sb1_m[csb+1] = p[(k==kmax-1) ? c+xy : c+xy + 1];
      sb1_b[csb+1] = p[(k==kmax-1) ? c+2*xy : c+2*xy + 1];
    }
    if(threadIdx.y==0){
      sb1_t[csb-blockDim.x-2] = p[(j==0) ? c : c - kmax];
      sb1_m[csb-blockDim.x-2] = p[(j==0) ? c+xy : c+xy - kmax];
      sb1_b[csb-blockDim.x-2] = p[(j==0) ? c+2*xy : c+2*xy - kmax];
    }
    if(threadIdx.y==blockDim.y-1){
      sb1_t[csb+blockDim.x+2] = p[(j==jmax-1) ? c : c + kmax];
      sb1_m[csb+blockDim.x+2] = p[(j==jmax-1) ? c+xy : c+xy + kmax];
      sb1_b[csb+blockDim.x+2] = p[(j==jmax-1) ? c+2*xy : c+2*xy + kmax];
    }
    if(threadIdx.x==0&&threadIdx.y==0){
      sb1_t[0] = p[(k==0||j==0) ? c : c-kmax-1];
      sb1_t[blockDim.x+1] = p[(k+blockDim.x==kmax||j==0) ? c : c-kmax+blockDim.x];
      sb1_t[(blockDim.y+1)*(blockDim.x+2)] = p[(k==0||j+blockDim.y==jmax) ? c : c+blockDim.y*kmax-1];
      sb1_t[(blockDim.y+2)*(blockDim.x+2)-1] = p[(k+blockDim.x==kmax||j+blockDim.y==jmax) ? c : c+blockDim.y*kmax+blockDim.x];
      sb1_m[0] = p[(k==0||j==0) ? c+xy : c+xy-kmax-1];
      sb1_m[blockDim.x+1] = p[(k+blockDim.x==kmax||j==0) ? c+xy : c+xy-kmax+blockDim.x];
      sb1_m[(blockDim.y+1)*(blockDim.x+2)] = p[(k==0||j+blockDim.y==jmax) ? c+xy : c+xy+blockDim.y*kmax-1];
      sb1_m[(blockDim.y+2)*(blockDim.x+2)-1] = p[(k+blockDim.x==kmax||j+blockDim.y==jmax) ? c+xy : c+xy+blockDim.y*kmax+blockDim.x];
      sb1_b[0] = p[(k==0||j==0) ? c+2*xy : c+2*xy-kmax-1];
      sb1_b[blockDim.x+1] = p[(k+blockDim.x==kmax||j==0) ? c+2*xy : c+2*xy-kmax+blockDim.x];
      sb1_b[(blockDim.y+1)*(blockDim.x+2)] = p[(k==0||j+blockDim.y==jmax) ? c+2*xy : c+2*xy+blockDim.y*kmax-1];
      sb1_b[(blockDim.y+2)*(blockDim.x+2)-1] = p[(k+blockDim.x==kmax||j+blockDim.y==jmax) ? c+2*xy : c+2*xy+blockDim.y*kmax+blockDim.x];
    }
    __syncthreads();
    sb2_t[csb2] = sb1_t[csb];
    i = 1;
    s0 =
				a0[i*jmax*kmax+j*kmax+k] * sb1_b[csb]
				+ a1[i*jmax*kmax+j*kmax+k] * sb1_m[csb + blockDim.x + 2]
				+ a2[i*jmax*kmax+j*kmax+k] * sb1_m[csb + 1]
				+ b0[i*jmax*kmax+j*kmax+k] * (
				  sb1_b[csb + blockDim.x + 2]
        - sb1_b[csb - blockDim.x - 2]
        - sb1_t[csb + blockDim.x + 2]
        + sb1_t[csb - blockDim.x - 2])
        + b1[i*jmax*kmax+j*kmax+k] *(
				  sb1_m[csb + blockDim.x + 3]
        - sb1_m[csb - blockDim.x - 1]
        - sb1_m[csb - blockDim.x - 3]
        + sb1_m[csb + blockDim.x + 1])
        + b2[i*jmax*kmax+j*kmax+k] *(
				  sb1_b[csb + 1]
        - sb1_t[csb + 1]
        - sb1_b[csb - 1]
        + sb1_t[csb - 1])
        + c0[i*jmax*kmax+j*kmax+k] * sb1_t[csb]
				+ c1[i*jmax*kmax+j*kmax+k] * sb1_m[csb - blockDim.x - 2]
				+ c2[i*jmax*kmax+j*kmax+k] * sb1_m[csb - 1]
				+ wrk1[i*jmax*kmax+j*kmax+k];

		ss = ( s0 * a3[i*jmax*kmax+j*kmax+k] - sb1_m[csb] ) * bnd[i*jmax*kmax+j*kmax+k];

		temp = temp + ss*ss;

		wrk2[i*jmax*kmax+j*kmax+k] = (k==0||k==kmax-1||j==0||j==jmax-1) ? sb1_m[csb] : (sb1_m[csb] + omega * ss);

    sb2_m[csb2] = wrk2[i*jmax*kmax+j*kmax+k];
		c += 2*xy;
    __syncthreads();

		for(i=2 ; i<imax-1 ; ++i){
      float *sb_tmp = sb1_t;
      sb1_t = sb1_m;
      sb1_m = sb1_b;
      sb1_b = sb_tmp;
      sb1_b[csb] = p[c+xy];
      if(threadIdx.x == 0){ sb1_b[csb-1] = p[(k==0) ? c+xy : c+xy-1];}
      if(threadIdx.x == blockDim.x-1){ sb1_b[csb+1] = p[(k==kmax-1) ? c+xy : c+xy+1];}
      if(threadIdx.y == 0){ sb1_b[csb-blockDim.x-2] = p[(j==0) ? c+xy : c+xy-kmax];}
      if(threadIdx.y == blockDim.y-1){ sb1_b[csb+blockDim.x+2] = p[(j==jmax-1) ? c+xy : c+xy+kmax];}
			if(threadIdx.x == 0 && threadIdx.y == 0){
        sb1_b[0] = p[(k==0||j==0) ? c+xy : c+xy-kmax-1];
        sb1_b[blockDim.x+2] = p[(k+blockDim.x==kmax||j==0) ? c+xy : c+xy-kmax+blockDim.x];
        sb1_b[(blockDim.y+1)*(blockDim.x+2)] = p[(k==0||j+blockDim.y==jmax) ? c+xy : c+xy+blockDim.y*kmax-1];
        sb1_b[(blockDim.y+2)*(blockDim.x+2)] = p[(k+blockDim.x==kmax||j+blockDim.y==jmax) ? c+xy : c+xy+blockDim.y*kmax+blockDim.x];
      }
      __syncthreads();

      s0 =
				a0[i*jmax*kmax+j*kmax+k] * sb1_b[csb]
				+ a1[i*jmax*kmax+j*kmax+k] * sb1_m[csb + blockDim.x + 2]
				+ a2[i*jmax*kmax+j*kmax+k] * sb1_m[csb + 1]
				+ b0[i*jmax*kmax+j*kmax+k] * (
				  sb1_b[csb + blockDim.x + 2]
        - sb1_b[csb - blockDim.x - 2]
        - sb1_t[csb + blockDim.x + 2]
        + sb1_t[csb - blockDim.x - 2])
        + b1[i*jmax*kmax+j*kmax+k] *(
				  sb1_m[csb + blockDim.x + 3]
        - sb1_m[csb - blockDim.x - 1]
        - sb1_m[csb - blockDim.x - 3]
        + sb1_m[csb + blockDim.x + 1])
        + b2[i*jmax*kmax+j*kmax+k] *(
				  sb1_b[csb + 1]
        - sb1_t[csb + 1]
        - sb1_b[csb - 1]
        + sb1_t[csb - 1])
        + c0[i*jmax*kmax+j*kmax+k] * sb1_t[csb]
				+ c1[i*jmax*kmax+j*kmax+k] * sb1_m[csb - blockDim.x - 2]
				+ c2[i*jmax*kmax+j*kmax+k] * sb1_m[csb - 1]
				+ wrk1[i*jmax*kmax+j*kmax+k];

		  ss = ( s0 * a3[i*jmax*kmax+j*kmax+k] - sb1_b[csb] ) * bnd[i*jmax*kmax+j*kmax+k];

		  temp = temp + ss*ss;

		  wrk2[i*jmax*kmax+j*kmax+k] = (k==0||k==kmax-1||j==0||j==jmax-1) ? sb1_m[csb] : (sb1_b[csb] + omega * ss);

      sb2_b[csb2] = wrk2[i*jmax*kmax+j*kmax+k];

	  	__syncthreads();

      if(0<threadIdx.x && threadIdx.x<blockDim.x-1 && 0<threadIdx.y && threadIdx.y<blockDim.y-1){
		    s0 =
				a0[i*jmax*kmax+j*kmax+k] * sb2_b[csb2]
				+ a1[i*jmax*kmax+j*kmax+k] * sb2_m[csb2 + blockDim.x]
				+ a2[i*jmax*kmax+j*kmax+k] * sb2_m[csb2 + 1]
				+ b0[i*jmax*kmax+j*kmax+k] * (
				  sb2_b[csb2 + blockDim.x]
        - sb2_b[csb2 - blockDim.x]
        - sb2_t[csb2 + blockDim.x]
        + sb2_t[csb2 - blockDim.x])
        + b1[i*jmax*kmax+j*kmax+k] *(
				  sb2_m[csb2 + blockDim.x]
        - sb2_m[csb2 - blockDim.x]
        - sb2_m[csb2 - blockDim.x]
        + sb2_m[csb2 + blockDim.x])
        + b2[i*jmax*kmax+j*kmax+k] *(
				  sb2_b[csb2 + 1]
        - sb2_t[csb2 + 1]
        - sb2_b[csb2 - 1]
        + sb2_t[csb2 - 1])
        + c0[i*jmax*kmax+j*kmax+k] * sb2_t[csb2]
				+ c1[i*jmax*kmax+j*kmax+k] * sb2_m[csb2 - blockDim.x]
				+ c2[i*jmax*kmax+j*kmax+k] * sb2_m[csb2 - 1]
				+ wrk1[i*jmax*kmax+j*kmax+k];


		  ss = ( s0 * a3[i*jmax*kmax+j*kmax+k] - sb2_m[csb2] ) * bnd[i*jmax*kmax+j*kmax+k];

	  	temp = temp + ss*ss;

		  p[i*jmax*kmax+j*kmax+k] = sb2_m[csb2] + omega * ss;
      }
		c += xy;
    __syncthreads();

    sb_tmp = sb2_t;
    sb2_t = sb2_m;
    sb2_m = sb2_b;
    sb2_b = sb_tmp;
		}


    sb2_b[csb2] = sb1_b[csb];
    if(0<threadIdx.x && threadIdx.x<blockDim.x-1 && 0<threadIdx.y && threadIdx.y<blockDim.y-1){
		  s0 =
				a0[i*jmax*kmax+j*kmax+k] * sb2_b[csb2]
				+ a1[i*jmax*kmax+j*kmax+k] * sb2_m[csb2 + blockDim.x]
				+ a2[i*jmax*kmax+j*kmax+k] * sb2_m[csb2 + 1]
				+ b0[i*jmax*kmax+j*kmax+k] * (
				  sb2_b[csb2 + blockDim.x]
        - sb2_b[csb2 - blockDim.x]
        - sb2_t[csb2 + blockDim.x]
        + sb2_t[csb2 - blockDim.x])
        + b1[i*jmax*kmax+j*kmax+k] *(
				  sb2_m[csb2 + blockDim.x]
        - sb2_m[csb2 - blockDim.x]
        - sb2_m[csb2 - blockDim.x]
        + sb2_m[csb2 + blockDim.x])
        + b2[i*jmax*kmax+j*kmax+k] *(
				  sb2_b[csb2 + 1]
        - sb2_t[csb2 + 1]
        - sb2_b[csb2 - 1]
        + sb2_t[csb2 - 1])
        + c0[i*jmax*kmax+j*kmax+k] * sb2_t[csb2]
				+ c1[i*jmax*kmax+j*kmax+k] * sb2_m[csb2 - blockDim.x]
				+ c2[i*jmax*kmax+j*kmax+k] * sb2_m[csb2 - 1]
				+ wrk1[i*jmax*kmax+j*kmax+k];


		  ss = ( s0 * a3[i*jmax*kmax+j*kmax+k] - sb2_m[csb2] ) * bnd[i*jmax*kmax+j*kmax+k];

	  	temp = temp + ss*ss;

		  p[i*jmax*kmax+j*kmax+k] = sb2_m[csb2] + omega * ss;
    }
    __syncthreads();
  } /* end n loop */
	__syncthreads();
	gosa[tid] = temp;
}

int main(){
	int i, j, k;
	float final_gosa;
	double cpu0, cpu1, nflop, xmflops2, score;

	float gosa[THREAD_NUM];

	/************************************/
	float *p;
	float *a0, *a1, *a2, *a3;
	float *b0, *b1, *b2;
	float *c0, *c1, *c2;

	float *bnd;
	float *wrk1, *wrk2;
	/************************************/
	mimax = MIMAX;
	mjmax = MJMAX;
	mkmax = MKMAX;
	imax = MIMAX-1;
	jmax = MJMAX-1;
	kmax = MKMAX-1;
	//int N_IJK = MIMAX*MJMAX*MKMAX;
	int N_IJK = mimax*mjmax*mkmax;
	int WORKSIZE = THREAD_NUM*mimax;
	/************************************/
	float *dev_p;
	float *dev_a0, *dev_a1, *dev_a2, *dev_a3;
	float *dev_b0, *dev_b1, *dev_b2;
	float *dev_c0, *dev_c1, *dev_c2;

	float *dev_bnd;
	float *dev_wrk1, *dev_wrk2;

	float *dev_gosa;
	/************************************/


	omega = 0.8;

	//initial_maxtrix();

	/******allocate mem on CPU***********/
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
	/************************************/

	/******allocate mem on GPU***********/
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

	cudaMalloc((void**)&dev_gosa, sizeof(float)*THREAD_NUM);
	/************************************/

	/*****Initialize*********************/
	//int i,j,k;
	/*
	for(i=0 ; i<mimax ; ++i)
		for(j=0 ; j<mjmax ; ++j)
			for(k=0 ; k<mkmax ; ++k){
				a0[i*mjmax*mkmax+j*mkmax+k]=0.0;
				a1[i*mjmax*mkmax+j*mkmax+k]=0.0;
				a2[i*mjmax*mkmax+j*mkmax+k]=0.0;
				a3[i*mjmax*mkmax+j*mkmax+k]=0.0;
				b0[i*mjmax*mkmax+j*mkmax+k]=0.0;
				b1[i*mjmax*mkmax+j*mkmax+k]=0.0;
				b2[i*mjmax*mkmax+j*mkmax+k]=0.0;
				c0[i*mjmax*mkmax+j*mkmax+k]=0.0;
				c1[i*mjmax*mkmax+j*mkmax+k]=0.0;
				c2[i*mjmax*mkmax+j*mkmax+k]=0.0;
				p[i*mjmax*mkmax+j*mkmax+k]=0.0;
				wrk1[i*mjmax*mkmax+j*mkmax+k]=0.0;
				bnd[i*mjmax*mkmax+j*mkmax+k]=0.0;
	      		}
	*/
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
        wrk2[i*mjmax*mkmax+j*mkmax+k]=0.0;
				bnd[i*mjmax*mkmax+j*mkmax+k]=1.0;
			}
		}
	}
	/************************************/

	/*****copy array to device mem*******/
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
	/************************************/

	printf("mimax = %d mjmax = %d mkmax = %d\n", MIMAX, MJMAX, MKMAX);
	printf("imax = %d jmax = %d kmax = %d\n", imax, jmax, kmax);

	cpu0 = second(); /**measuring**/

	dim3 block(BLOCKSIZEX, BLOCKSIZEY, 1);
	dim3 grid(GRIDSIZEX, GRIDSIZEY, 1);

	jacobi<<<grid, block, sizeof(float)*3*(BLOCKSIZE+(BLOCKSIZEX+2)*(BLOCKSIZEY+2))>>>(dev_a0, dev_a1, dev_a2, dev_a3, dev_b0, dev_b1, dev_b2, dev_c0, dev_c1, dev_c2, dev_p, dev_wrk1, dev_wrk2, dev_bnd, NN, mimax, mjmax, mkmax, omega, dev_gosa);

  //for(int i = 0; i<N_IJK; i++){
  //  printf("%f ", p[i]);
  //}
	cudaDeviceSynchronize();

	cpu1 = second();

	cudaMemcpy(&gosa, dev_gosa, sizeof(float)*THREAD_NUM, cudaMemcpyDeviceToHost);

	/******Free mem on the GPU**********/
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
	/************************************/

	/********Final sum of gosa***********/
	for(int gosa_index=0; gosa_index<THREAD_NUM; gosa_index++){
		//printf("%f\n", gosa[gosa_index]);
		final_gosa += gosa[gosa_index];
		//printf("Gosa%d: %e \n", gosa_index, gosa[gosa_index]);
	}
	/************************************/

	nflop = (kmax-2)*(jmax-2)*(imax-2)*34;

	if(cpu1 != 0.0){
		xmflops2 = nflop/cpu1*1.0e-6*(float)NN;
	}

	score = xmflops2/32.27;

	printf("gpu: %f sec.\n", cpu1);
	printf("Loop executed for %d times\n", NN);
	printf("Gosa: %e \n", final_gosa);
	printf("MFLOPS measured: %f\n", xmflops2);
	//printf("Score: %f\n", score);

	return(0);
}
