#include <math.h>
#include <cstdlib>
#include <cstring>
#include "grain.h"
#include "sbsms.h"
#include "defs.h"
#include "real.h"

#include <map>
using namespace std;
using namespace _sbsms_;

map<int, map<int, map<real, real*> > > wwMap;
map<int, map<int, map<real, audio*> > > peakMap;

map<int, fftplan*> fftPlanMap;
map<int, fftplan*> ifftPlanMap;

long grain :: count = 0;

grain* grain :: create(int N,real pad,int wintype)
{
  grain *g = new grain(N,pad,wintype);
  
  g->refCount = 0;
  g->time = (audio*) malloc(N*sizeof(audio));
  g->freq = (audio*) malloc(N*sizeof(audio));
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
  free_audio_buf(g->time);
  free_audio_buf(g->freq);
  delete g;
}

grain :: grain(int N, real pad, int type)
{ 
  this->type = type;
  this->N = N;
  this->pad = pad;
  calc_plans();
  calc_windows();
}

void grain :: analyze() 
{
  for(int k=0;k<N;k++) {
    for(int c=0;c<2;c++) {
      freq[k][c] = time[k][c] * ww[k];
    }
  }
  FFT(fftPlan,freq);
}

void grain :: synthesize() 
{
  memcpy(time,freq,N*sizeof(audio));

  IFFT(ifftPlan,time);

  real f = pad/(real)N;
    
  for(int c=0;c<2;c++) {
    for(int k=0;k<N;k++) {
      time[k][c] *= ww[k] * f;
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
  ww = wwMap[type][N][pad];
  if(ww == NULL) {
    wwMap[type][N][pad] = (ww = (real*)calloc(N,sizeof(real)));   
    for(int k=0;k<N2;k++) {
      if(type==SBSMS_HANN)
	ww[k+(N-N2)/2] = .5*(1.0 - cos((real)k/(real)N2*TWOPI));
      else if(type==SBSMS_HAMMING)
	ww[k+(N-N2)/2] = .53836 - .46164*cos((real)k/(real)N2*TWOPI);
    }
  }

  peak = peakMap[type][N][pad];
  if(peak == NULL) {
    peakMap[type][N][pad] = (peak = (audio*) malloc(N*sizeof(audio)));
    for(int k=0;k<N;k++) {
      peak[k][0] = ww[k]/(real)(N2/2);
      peak[k][1] = 0;
    }
    FFT(fftPlan,peak);
  }
}

grain* grain :: upsample()
{
  grain *g2 = grain::create(2*N,pad,type);
  grain *g = this;

  for(int c=0;c<2;c++) {
    for(int k=0;k<=N/2;k++)
      g2->freq[k][c] = g->freq[k][c];

    for(int k=N/2+1;k<=N/2+N;k++)
      g2->freq[k][c] = 0;

    for(int k=N/2+N+1;k<2*N;k++)
      g2->freq[k][c] = g->freq[k-N][c];
  }
  return g2;
}

grain* grain :: downsample()
{
  grain *g2 = grain::create(N/2,pad,type);
  grain *g = this;

  for(int c=0;c<2;c++) {
    for(int k=0;k<=N/4-1;k++)
      g2->freq[k][c] = g->freq[k][c];
    
    g2->freq[N/4][c] = 0.5*(g->freq[N/4][c] + g->freq[N-N/4][c] );
    
    for(int k=N/4+1;k<N/2;k++)
      g2->freq[k][c] = g->freq[k+N/2][c];
  }

  return g2;
}

grain *grain :: lpfilter()
{
  grain *g = this;
  for(int c=0;c<2;c++) {
    for(int k=N/4+1;k<=N-N/4-1;k++)
      g->freq[k][c] = 0;
  }
  return this;
}


//destructive
grain *grain :: hpfilter()
{
  grain *g = this;
  for(int c=0;c<2;c++) {
    for(int k=0;k<=N/4;k++)
      g->freq[k][c] = 0;
    
    for(int k=N-N/4;k<N;k++)
      g->freq[k][c] = 0;
  }
  return this;
}
