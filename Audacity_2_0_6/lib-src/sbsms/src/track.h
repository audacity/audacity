// -*- mode: c++ -*-
#ifndef TRACK_H
#define TRACK_H

#include "trackpoint.h"
#include "config.h"

#ifdef MULTITHREADED
#include "pthread.h"
#endif

#include <vector>
using namespace std;

namespace _sbsms_ {

enum {
  trackIndexNone = 0
};

#define PhShift 5
#define WShift 21
#define Ph1 65535
#define WPI 536870912
#define W2PI 1073741824
#define W2PIMask 1073741823
#define WScale 1.708913188941079e8f
#define MScale 4.656683928435187e-10f

enum SynthMode {
  synthModeOutput = 0,
  synthModeTrial2,
  synthModeTrial1
};

class SMS;

class Track : public SBSMSTrack {
public:
  Track(float h, TrackIndexType index, TrackPoint *p, const TimeType &time, bool bStitch);
  ~Track();

  SBSMSTrackPoint *getSBSMSTrackPoint(const TimeType &time);
  TrackIndexType getIndex();
  bool isFirst(const TimeType &synthtime);
  bool isLast(const TimeType &synthtime);

protected:
  void push_back(TrackPoint *p);
  TrackPoint *back();
  void endTrack(bool bStitch);
  TrackPoint *getTrackPoint(const TimeType &time);
  TimeType size();
  TrackPoint *updateFPH(const TimeType &time, int mode, int n, float f0, float f1);
  void updateM(const TimeType &time, int mode);
  void step(const TimeType &time);
  void synth(float *out, const TimeType &synthtime, int n, int mode, int c);
  bool jump(TrackPoint *tp0, TrackPoint *tp1);
  void absorb();

  friend class SMS;
  friend class SynthRenderer;
  friend class TrackPoint;

 protected:
  vector<TrackPoint*> point;
  float h;
  float jumpThresh;
  TrackIndexType index;
  TimeType start;
  TimeType first;
  TimeType end;
  TimeType last;
  bool bEnd;
  bool bEnded;
  bool bRender;
  bool bStitch;
  bool bSplit;
  bool bMerge;
};

}

#endif
