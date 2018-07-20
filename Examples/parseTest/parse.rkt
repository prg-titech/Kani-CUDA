#lang racket

(require c/parse)

(parse-program "void diffusion_kernel(int *f1,
                                 int *f2,
                                 int nx, int ny, int nz,
                                 int ce, int cw, int cn, int cs,
                                 int ct, int cb, int cc) {
  int i = blockDim.x * blockIdx.x + threadIdx.x;  
  int j = blockDim.y * blockIdx.y + threadIdx.y;
  int c = i + j * nx;
  int xy = nx * ny;
  for (int k = 0; k < nz; ++k) {
    int w = (i == 0)        ? c : c - 1;
    int e = (i == nx-1)     ? c : c + 1;
    int n = (j == 0)        ? c : c - nx;
    int s = (j == ny-1)     ? c : c + nx;
    int b = (k == 0)        ? c : c - xy;
    int t = (k == nz-1)     ? c : c + xy;
//#if 1
    f2[c] = cc * f1[c] + cw * f1[w] + ce * f1[e] + cs * f1[s]
        + cn * f1[n] + cb * f1[b] + ct * f1[t];
//#else
    // simulating the ordering of shared memory version
    int v = cc * f1[c];
    v += cw * f1[w];
    v += ce * f1[e];
    v += cs * f1[s];
    v += cn * f1[n];
    v += cb * f1[b] + ct * f1[t];
    f2[c] = v;
//#endif    
    c += xy;
  }
  return;
}")