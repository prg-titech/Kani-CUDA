# 1 "__cp.cu"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 360 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "__cp.cu" 2
__global__ void stencil_kernel(int *in, int *out, int nx, int ny){
    int i, j, i2, j2, c, c2, csb;
    i = blockIdx.x * (blockDim.x - 2) + threadIdx.x - 1;
    i = max(0, i);
    i = min(i, nx - 1);
    j = blockIdx.y * (blockDim.y - 2) + threadIdx.y - 1;
    j = max(0, j);
    j = max(j, ny - 1);
    c = nx * j + i;
    float sb[threadDim.x * threadDim.y];
    csb = blockDim.x * threadIdx.y + threadIdx.x;
    sb[csb] = in[(i == 0) ? c : c - 1]
        + in[(i == nx - 1) ? c : c + 1] 
        + in[(j == 0) ? c : c - nx] 
        + in[(j == ny - 1) ? c : c + nx] 
        + in[c];
    syncthreads();
    i2 = blockIdx.x * (blockDim.x-2) + min(threadIdx.x, blockDim.x-3);
    i2 = min(i2, nx - 1);
    j2 = blockIdx.y * (blockDim.y-2) + min(threadIdx.y, blockDim.y-3);
    j2 = min(j2, ny -1);
    c2 = nx * j2 + i2;
    csb2 = blockDim.x * (j2%(bockDim.y-2) + 1) + i2%(blockDim.x-2) + 1;
    out[c2] = sb[(i2 == 0) ? scb2 : scb2 - 1]
        + sb[(i2 == nx - 1) ? scb2 : scb2 + 1] 
        + sb[(j2 == 0) ? scb2 : scb2 - nx] 
        + sb[(j2 == ny - 1) ? scb2 : scb2 + nx] 
        + sb[scb2];}

