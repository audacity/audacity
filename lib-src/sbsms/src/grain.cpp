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

map<int, map<real, real*> > wwMap;
map<int, map<real, audio*> > peakMap;

map<int, fftplan*> fftPlanMap;
map<int, fftplan*> ifftPlanMap;

grain* grain :: create(int N,real pad)
{
  grain *g = new grain(N,pad);
  return g;
}

void grain :: referenced(grain *g)
{
  g->refCount++;
}

void grain :: forget(grain *g)
{
  g->refCount--;
  if(g->refCount <= 0) {
    destroy(g);
  }
}

void grain :: destroy(grain *g)
{
  free_audio_buf(g->x);
  delete g;
}

grain :: grain(int N, real pad)
{ 
  this->N = N;
  this->pad = pad;
  calc_plans();
  calc_windows();
  refCount = 0;
  x = (audio*) malloc(N*sizeof(audio));
}

void grain :: analyze() 
{
  for(int k=0;k<N;k++) {
    for(int c=0;c<2;c++) {
      x[k][c] *= ww[k];
    }
  }
  FFT(fftPlan,x);
}

void grain :: synthesize() 
{
  IFFT(ifftPlan,x);
  real f = pad/(real)N;
  for(int k=0;k<N;k++) {
    for(int c=0;c<2;c++) {
      x[k][c] *= ww[k] * f;
    }
  }
}

void grain :: calc_plans()
{
  fftPlan = fftPlanMap[N];
  if(fftPlan == NULL) {
    fftPlanMap[N] = (fftPlan = planFFT(N)); 
  }
     
  ifftPlan = ifftPlanMap[N];
  if(ifftPlan == NULL) {
    ifftPlanMap[N] = (ifftPlan = planIFFT(N)); 
  }
}
 
void grain :: calc_windows() 
{  
  int N2 = round2int(N/pad);
  ww = wwMap[N][pad];
  if(ww == NULL) {
    wwMap[N][pad] = (ww = (real*)calloc(N,sizeof(real)));   
    for(int k=0;k<N2;k++) {
      ww[k+(N-N2)/2] = .5*(1.0 - cos((real)k/(real)N2*TWOPI));
    }
  }

  peak = peakMap[N][pad];
  if(peak == NULL) {
    peakMap[N][pad] = (peak = (audio*) calloc(N,sizeof(audio)));
    for(int k=0;k<N;k++) {
      peak[k][0] = ww[k]/(real)(N2/2);
    }
    FFT(fftPlan,peak);
  }
}

grain* grain :: upsample()
{
  grain *g2 = grain::create(2*N,pad);
  grain *g = this;

  for(int c=0;c<2;c++) {
    for(int k=0;k<=N/2;k++)
      g2->x[k][c] = g->x[k][c];

    for(int k=N/2+1;k<=N/2+N;k++)
      g2->x[k][c] = 0;

    for(int k=N/2+N+1;k<2*N;k++)
      g2->x[k][c] = g->x[k-N][c];
  }
  return g2;
}

grain* grain :: downsample()
{
  grain *g2 = grain::create(N/2,pad);
  grain *g = this;

  for(int c=0;c<2;c++) {
    for(int k=0;k<=N/4-1;k++)
      g2->x[k][c] = g->x[k][c];
    
    g2->x[N/4][c] = 0.5*(g->x[N/4][c] + g->x[N-N/4][c] );
    
    for(int k=N/4+1;k<N/2;k++)
      g2->x[k][c] = g->x[k+N/2][c];
  }

  return g2;
}

}
