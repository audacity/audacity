#include <math.h>
#include "real.h"
#include "utils.h"
#include "track.h"
#include "trackpoint.h"
#include "grain.h"
#include "defs.h"

namespace _sbsms_ {

trackpoint :: trackpoint(trackpoint *tp)
{
  init();
  f = tp->f;
  y = tp->y;
  y0 = tp->y0;
  h = tp->h;
  M = tp->M;
  ph = tp->ph;
  owner = tp->owner;
}

trackpoint :: ~trackpoint()
{
  for(int d=0;d<3;d++) {
    if(dup[d]) {
      dup[d]->dup[2-d] = NULL;
    }
  }
}

trackpoint :: trackpoint(grain *g, real x, real y, int N, short M, short h, long time)
{
  init();
  real k = x;
  int ki = round2int(k);
  real kf = ki<k?k-ki:ki-k;

  real norm0 = square(g->freq[ki][0]) + square(g->freq[ki][1]); 
  real ph0;
  if(norm0 > 0.0f) {
    ph0 = atan2(g->freq[ki][1],g->freq[ki][0]);
  } else {
    ph0 = 0.0f;
  }

  int ki1 = ki<k?ki+1:ki-1;
  real norm1, ph1;
  if(ki == N-1) {
    norm1 = norm0;
    ph1 = ph0;
  } else {
    norm1 = square(g->freq[ki1][0]) + square(g->freq[ki1][1]);
    if(norm1 > 0.0f) {
      ph1 = atan2(g->freq[ki1][1],g->freq[ki1][0]);
    } else { 
      ph1 = 0.0f;
    }
  }

  real ifreq = TWOPI*k/(real)N;
  ph0 = ph0 + (real)ki*PI;
  ph1 = ph1 + (real)ki1*PI;
  if(kf < 0.5) {
    real dp = canon(ph1 - ph0);
    ph1 = ph0 + dp;
  } else {
    real dp = canon(ph0 - ph1);
    ph0 = ph1 + dp;
  }
  real ph = ((1.0-kf)*ph0 + kf*ph1);

  this->time = time;
  this->y = y;
  this->y0 = y;
  this->M = M;
  this->h = h;
  this->f = ifreq;
  this->ph = canon(ph);
}

trackpoint :: trackpoint()
{
  init();
}

void trackpoint :: init()
{
  for(int d=0;d<3;d++) 
    dup[d] = NULL; 
  contF = 1e9;
  cont = NULL;
  dupcont = NULL;
  bConnect = false;
  bConnected = false;
  bDelete = false;
  owner = NULL;
}

}
