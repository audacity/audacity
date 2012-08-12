// -*- mode: c++ -*-
#ifndef TRACKPOINT_H
#define TRACKPOINT_H

#include "sbsms.h"

namespace _sbsms_ {

enum { TrackPointNoCont = 65535 };

class Track;
class Slice;

class TrackPoint : public SBSMSTrackPoint {
public:
  TrackPoint(Slice *slice, float *peak, audio *gx, float *mag, float *mag2, int k, int N, int band);
  ~TrackPoint();
  void destroy();
  float getF();
  float getM();
  float getPhase();
  void absorb();
protected:
  TrackPoint *pp;
  TrackPoint *pn;
  TrackPoint *dupcont;
  TrackPoint *dupStereo;
  TrackPoint *cont;
  TrackPoint *dup[3];
  Track *owner;
  Slice *slice;
  float *peak;
  float x01;
  float y01;
  float phSynth;
  union {
    float fSynth0;
    float xtp2;
  };
  union {
    float fSynth1;
    float xtn2;
  };
  int refCount;
  float f;
  float x;
  float y;
  float ph;
  float contF;
  float m;
  float m2;
  bool bJump;
  bool bSyncStereo;
  bool bConnected;
  bool bConnect;
  bool bDelete;
  bool bOwned;
  bool bMarked;
  bool bSplit;
  bool bMerge;

  friend class Slice;
  friend class SMS;
  friend class Track;
};

class Slice {
public:
  Slice(int band, const TimeType &time);
  ~Slice();
  void remove(TrackPoint *tp);
  TrackPoint *bottom;
  TrackPoint *top;
  int band;
  TimeType time;
};

}

#endif
