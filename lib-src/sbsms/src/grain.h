#ifndef GRAIN_H
#define GRAIN_H

#include "fft.h"
#include "audio.h"
#include "sbsms.h"

namespace _sbsms_ {

class grain {
 public:
  static grain* create(int N, real pad);
  static void destroy(grain *g);
  static void referenced(grain *g);
  static void forget(grain *g);

  t_fft *x;
  int N;
  int h;
  real pad;
  int refCount;
  audio *peak;
 
  void analyze();
  void synthesize();
  grain* upsample();
  grain* downsample();
  
 protected:
  grain(int N,real pad);
  void calc_plans();
  void calc_windows();
  real *ww;
  fftplan *fftPlan, *ifftPlan;
};

}

#endif
