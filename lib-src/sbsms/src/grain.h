#ifndef GRAIN_H
#define GRAIN_H

#include "fft.h"
#include "audio.h"
#include "sbsms.h"

#define SBSMS_HANN 0
#define SBSMS_HAMMING 1

namespace _sbsms_ {

class grain {

 public:

  static long count;
  static grain* create(int N, real pad, int wintype);
  static void destroy(grain *g);
  static void referenced(grain *g);
  static void forget(grain *g);

  t_fft *time;
  t_fft *freq;
  int N, type, h;
  real pad;
  int refCount;
 
  void analyze();
  void synthesize();

  grain* upsample();
  grain* downsample();
  grain* lpfilter();
  grain* hpfilter();
  audio *peak;
  
 protected:
  grain(int N,real q,int wintype);
  void calc_windows();
  void calc_plans();
  real *ww;
  fftplan *fftPlan, *ifftPlan;
};

}

#endif
