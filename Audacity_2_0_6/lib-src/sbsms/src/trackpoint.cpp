#include <math.h>
#include "real.h"
#include "utils.h"
#include "trackpoint.h"
#include "track.h"

namespace _sbsms_ {

void TrackPoint :: destroy()
{
  refCount--;
  if(refCount <= 0) {
    delete this;
  }
}

TrackPoint :: TrackPoint(Slice *slice, float *peak, audio *gx, float *mag, float *mag2, int k, int N, int band)
{
  refCount = 0;
  for(int d=0;d<3;d++) {
    dup[d] = NULL; 
  }
  dupStereo = NULL;
  bJump = false;
  y01 = 0.0f;
  pp = NULL;
  pn = NULL;
  bSyncStereo = false;
  bConnect = false;
  bConnected = false;
  bDelete = false;
  bOwned = false;
  bMarked = false;
  bSplit = false;
  bMerge = false;
  owner = NULL;
  this->slice = slice;
  this->peak = peak;
  float y0 = mag[k-1];
  float y1 = mag[k];
  float y2 = mag[k+1];
  float d = (y0 + y2 - y1 - y1);
  x = (d==0.0f?k:k + 0.5f * (y0 - y2) / d);
  int ki = lrintf(x);
  int ki1;
  float kf;
  if(ki<x) {
    ki1 = ki + 1;
    kf = x - ki;
  } else {
    ki1 = ki - 1;
    kf = ki - x;
  }
  y = ((1.0f-kf)*mag2[ki] + kf*mag2[ki1]);
  f = TWOPI*x/(float)(N*(1<<band));
  float norm0 = square(gx[ki][0]) + square(gx[ki][1]); 
  float ph0;
  if(norm0 > 0.0f) {
    ph0 = atan2(gx[ki][1],gx[ki][0]);
  } else {
    ph0 = 0.0f;
  }
  float ph1;
  float norm1 = square(gx[ki1][0]) + square(gx[ki1][1]);
  if(norm1 > 0.0f) {
    ph1 = atan2(gx[ki1][1],gx[ki1][0]);
  } else { 
    ph1 = 0.0f;
  }
  ph0 += (float)(ki&1)*PI;
  ph1 += (float)(ki1&1)*PI;
  if(kf < 0.5f) {
    ph1 = ph0 + canonPI(ph1 - ph0);
  } else {
    ph0 = ph1 + canonPI(ph0 - ph1);
  }
  ph = canon2PI((1.0f-kf)*ph0 + kf*ph1);
  phSynth = ph;
}

void TrackPoint :: absorb()
{
  if(pp && pn) {
    if(pp->y * peak[lrintf(pp->x - x)] > pn->y * peak[lrintf(pn->x - x)]) {
      pp->m2 += m2;
    } else {
      pn->m2 += m2;
    }
  } else if(pp) {
    if(y01 == 0.0f || y01 * peak[lrintf(x01 - x)] < pp->y * peak[lrintf(pp->x - x)]) {      
      pp->m2 += m2;
    }
  } else if(pn) {
    if(y01 == 0.0f || y01 * peak[lrintf(x01 - x)] < pn->y * peak[lrintf(pn->x - x)]) {      
      pn->m2 += m2;
    }
  }
}

TrackPoint :: ~TrackPoint()
{
  for(int d=0;d<3;d++) {
    if(dup[d]) {
      dup[d]->dup[2-d] = NULL;
    }
  }
  if(slice) slice->remove(this);
  if(pp && pn) {
    pp->pn = pn;
    pn->pp = pp;
  } else if(pp) {
    pp->pn = NULL;  
  } else if(pn) {
    pn->pp = NULL;
  }
}

float TrackPoint :: getF()
{
  return f;
}

float TrackPoint :: getM()
{
  return y;
}

float TrackPoint :: getPhase()
{
  return ph;
}

Slice :: Slice(int band, const TimeType &time)
{
  this->band = band;
  this->time = time;
  bottom = NULL;
  top = NULL;
}

void Slice :: remove(TrackPoint *tp)
{
  if(tp == top) {
    top = top->pp;
  }
  if(tp == bottom) {
    bottom = bottom->pn;
  }
}

Slice :: ~Slice()
{
  for(TrackPoint *tp = bottom;
      tp;
      tp = tp->pn) {
    tp->slice = NULL;
  }
}

}
