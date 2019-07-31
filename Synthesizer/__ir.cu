#include<stdio.h>
#include "stopwatch.h"

#define BLOCK_X 3
#define BLOCK_Y 4
#define GRID_X 3
#define GRID_Y 3
#define M_PI (3.1415926535897932384626)

__global__ void diffusion_kernel(float* in,
                                 float* out,
                                 int nx, int ny, int nz,
                                 float ce, float cw, float cn, float cs,
                                 float ct, float cb, float cc) {
  profile("threadIdx.x threadIdx.y blockDim.x blockDim.y csb c i j");
  int i = blockDim.x * blockIdx.x + threadIdx.x;  
  int j = blockDim.y * blockIdx.y + threadIdx.y;
  int c = i + j * nx;
  int xy = nx * ny;
  __shared__ float sb[BLOCK_X * BLOCK_Y];
  int csb = threadIdx.x + threadIdx.y * blockDim.x;
  for (int k = 0; k < nz; ++k) {
    sb[csb] = in[c];
    int w = (i == 0)        ? c : c - 1;
    int e = (i == nx-1)     ? c : c + 1;
    int n = (j == 0)        ? c : c - nx;
    int s = (j == ny-1)     ? c : c + nx;
    int b = (k == 0)        ? c : c - xy;
    int t = (k == nz-1)     ? c : c + xy;
    out[c] = 
        cc * in[c] 
      + cw * __opt__951353.in[w] 
      + ce * __opt__217004.in[e] 
      + cs * __opt__968837.in[s]
      + cn * __opt__636769.in[n] 
      + cb * in[b] 
      + ct * in[t];   
    c += xy;
  }
}

void initialize(float *buff, const int nx, const int ny, const int nz,
                const float kx, const float ky, const float kz,
                const float dx, const float dy, const float dz,
                const float kappa, const float time) {
  float ax = exp(-kappa*time*(kx*kx));
  float ay = exp(-kappa*time*(ky*ky));
  float az = exp(-kappa*time*(kz*kz));
  int jz;  
  for (jz = 0; jz < nz; jz++) {
    int jy;
    for (jy = 0; jy < ny; jy++) {
      int jx;
      for (jx = 0; jx < nx; jx++) {
        int j = jz*nx*ny + jy*nx + jx;
        float x = dx*((float)(jx + 0.5));
        float y = dy*((float)(jy + 0.5));
        float z = dz*((float)(jz + 0.5));
        float f0 = (float)0.125
          *(1.0 - ax*cos(kx*x))
          *(1.0 - ay*cos(ky*y))
          *(1.0 - az*cos(kz*z));
        buff[j] = __symbol();//f0;
      }
    }
  }
}


int main(){
  int count = 3;
  int nx, ny, nz;
  nx = BLOCK_X*GRID_X;
  ny = BLOCK_Y*GRID_Y;
  nz = 4;

  float l, kappa;
  float kx, ky, kz;
  float dx, dy, dz, dt;
  float ce, cw, cn, cs, ct, cb, cc;
  l = 1.0;
  kappa = 0.1;
  dx = l / nx;
  dy = l / ny;
  dz = l / nz;
  kx = 2.0 * M_PI;
  ky = 2.0 * M_PI;
  kz = 2.0 * M_PI;
  dt = 0.1 * dx * dy / kappa;
  ce = kappa * dt /(dx*dx);
  cw = kappa * dt /(dx*dx);
  cn = kappa * dt /(dy*dy);
  cs = kappa * dt /(dy*dy);
  ct = kappa * dt /(dz*dz);
  cb = kappa * dt /(dz*dz);
  cc = 1.0 - (ce+cw+cn+cs+ct+cb);

  float *in, *dev_in, *dev_out;
  int s = sizeof(float) * nx * ny * nz;
  in = (float *)malloc(s);
  initialize(in, nx, ny, nz,
             kx, ky, kz, dx, dy, dz,
             kappa, 0.0);
  cudaMalloc((void**)&dev_in, s);
  cudaMalloc((void**)&dev_out, s);
  cudaMemcpy(dev_in, in, s, cudaMemcpyHostToDevice);


  dim3 block(BLOCK_X, BLOCK_Y, 1);
  dim3 grid(GRID_X, GRID_Y, 1);

  /*
  for(int k=0; k<nz; k++){
    for(int j=0; j<ny; j++){
      for(int i=0; i<nx; i++){
        printf("%f ", in[i+j*nx+k*nx*ny]);
      }
      printf("\n");
    }
    printf("\n");
  }
  */

  //Stopwatch st;
  //StopwatchStart(&st);

  for(int i = 0; i < count; i++){
    diffusion_kernel<<<grid, block>>>(
      dev_in, dev_out, nx, ny, nz, ce, cw, cn, cs, ct, cb, cc);
    float *t;
    t = dev_in;
    dev_in = dev_out;
    dev_out = t;
  }
  cudaMemcpy(in, dev_in, s, cudaMemcpyDeviceToHost);

  //float elapsed_time = StopwatchStop(&st);

  printf("pass\n");
  //printf("kernel time: %f\n", elapsed_time);
  /*
  for(int k=0; k<nz; k++){
    for(int j=0; j<ny; j++){
      for(int i=0; i<nx; i++){
        printf("%f ", in[i+j*nx+k*nx*ny]);
      }
      printf("\n");
    }
    printf("\n");
  }
  */
  
  return(0);
}
