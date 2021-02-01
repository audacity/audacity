#include <math.h>
#include <cstdlib>
#include <cstring>
#include "grain.h"
#include "sbsms.h"
#include "real.h"
#include "utils.h"

#include <map>
using namespace std;

namespace _sbsms_ {

GrainAllocator :: GrainAllocator(int N, int N2, int type)
{
  this->N = N;
  this->N2 = N2;
  this->type = type;

  switch(N) {
  case 128:
    fftPlan = &fft128;
    ifftPlan = &ifft128;
    break;
  case 256:
    fftPlan = &fft256;
    ifftPlan = &ifft256;
    break;
  case 384:
    fftPlan = &fft384;
    //ifftPlan = &ifft384;
    break;
  case 512:
    fftPlan = &fft512;
    //ifftPlan = &ifft512;
    break;
  default:
    abort();
    break;
  }

  int k1 = (N-N2)/2;

  w = (float*)calloc(N,sizeof(float));
  for(int k=0;k<N2;k++) {
    if(type == hann) {
      w[k+k1] = 0.5f*(1.0f - cos((float)k/(float)N2*TWOPI));
    } else if(type == hannpoisson) {
      w[k+k1] = 0.5f*(1.0f - cos((float)k/(float)N2*TWOPI)) * exp(-2.0f*fabs((float)(k-N2/2))/(float)N2);
    }
  }

  W = (audio*) calloc(N,sizeof(audio));
  for(int k=0;k<N;k++) {
    W[k][0] = w[k] * 2.638502561913447f/(float)N2;
  }

  fftPlan(W);
}

GrainAllocator :: ~GrainAllocator()
{
  free(w);
  free(W);
}

grain* GrainAllocator :: create()
{
  grain *g = new grain(N,N2);
  g->refCount = 0;
  g->fftPlan = fftPlan;
  g->ifftPlan = ifftPlan;
  g->w = w;
  return g;
}

void GrainAllocator :: reference(grain *g)
{
  g->refCount++;
}

void GrainAllocator :: forget(grain *g)
{
  g->refCount--;
  if(g->refCount <= 0) {
    delete g;
  }
}

grain :: grain(int N, int N2)
{
  this->N = N;
  this->synthScale = 1.0f / (float)N2;
  x = (audio*) calloc(N,sizeof(audio));
}

grain :: ~grain()
{
  free(x);
}

void grain :: analyze()
{
  float *x = (float*)this->x;
  float *end = x + (N<<1);
  float *w = this->w;
  while(x != end) {
    *(x++) *= *w;
    *(x++) *= *(w++);
  }

  fftPlan(this->x);
}

void grain :: synthesize() 
{
  ifftPlan(x);
  for(int k=0;k<N;k++) {
    for(int c=0;c<2;c++) {
      x[k][c] *= w[k] * synthScale;
    }
  }
}

void grain :: downsample(grain *g2)
{
  grain *g = this;

  int N2 = N/2;
  int N4 = N/4;
  for(int c=0;c<2;c++) {
    for(int k=0;k<N4;k++) {
      g2->x[k][c] = 0.5f * g->x[k][c];
    }
    g2->x[N4][c] = 0.25f * (g->x[N4][c] + g->x[N-N4][c]);
    for(int k=N4+1;k<N2;k++) {
      g2->x[k][c] = 0.5f * g->x[k+N2][c];
    }
  }
}

}
