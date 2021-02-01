// -*- mode: c++ -*-
#ifndef GRAIN_H
#define GRAIN_H

#include "fft.h"
#include "sbsms.h"

namespace _sbsms_ {

enum windowType {
  hann,
  hannpoisson
};

class GrainAllocator;

class grain {
 public:
  ~grain();
  void analyze();
  void synthesize();
  void downsample(grain *gdown);
  audio *x;
  friend class GrainAllocator;
 protected:
  grain(int N, int N2);
  float *w;
  int N;
  float synthScale;
  int refCount;
  fftplan fftPlan;
  fftplan ifftPlan;
};

class GrainAllocator {
public:
  GrainAllocator(int N, int N2, int type);
  ~GrainAllocator();
  grain *create();
  void reference(grain *g);
  void forget(grain *g);
  friend class grain;
  friend class GrainBuf;
protected:
  int N;
  int N2;
  int type;
  float *w;
  audio *W;
  fftplan fftPlan;
  fftplan ifftPlan;
};

}

#endif
