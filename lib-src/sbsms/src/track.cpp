#include "track.h"
#include "real.h"
#include "dBTable.h"
#include "synthTable.h"
#include "utils.h"

#include <algorithm>
using namespace std;

namespace _sbsms_ {

Track :: Track(float h, TrackIndexType index, TrackPoint *p, const TimeType &time, bool bStitch)
{
  this->h = h;
  jumpThresh = 1.0e-5f * h;
  this->index = index;
  bEnd = false;
  bEnded = false;
  bRender = false;
  bSplit = false;
  bMerge = false;
  first = time;
  start = time;
  if(bStitch) {
    this->bStitch = true;
  } else {
    this->bStitch = false;
    if(start > 0) {
      start--;
    }
  }
  point.push_back(p);
  p->owner = this;
  p->refCount++;
  end = time;
  last = time;
}

Track :: ~Track() {
  for(vector<TrackPoint*>::iterator i = point.begin();
      i != point.end();
      ++i) {
    TrackPoint *tp = (*i);
    if(tp) {    
       tp->destroy();
    }
  }
}

TrackIndexType Track :: getIndex()
{
  return index;
}

bool Track :: isFirst(const TimeType &time)
{
  return (time == first);
}

bool Track :: isLast(const TimeType &time)
{
  return (time == last);
}

TimeType Track :: size()
{
  return point.size();
}

TrackPoint *Track :: back() 
{
  return point.back(); 
}

TrackPoint *Track :: getTrackPoint(const TimeType &time)
{
  return point[time - first];
}

SBSMSTrackPoint *Track :: getSBSMSTrackPoint(const TimeType &time)
{
  return getTrackPoint(time);
}

bool Track :: jump(TrackPoint *tp0, TrackPoint *tp1)
{
  if(tp1->m > tp0->m) {
    float cost = 1.0e-4f * dBApprox(tp0->m,tp1->m);
    return (cost > jumpThresh);
  } else {
    return false;
  }
}
          
TrackPoint *Track :: updateFPH(const TimeType &time, int mode, int n, float f0, float f1)
{
  if(time == start && time < first) {
    TrackPoint *tp1 = getTrackPoint(time+1);
    tp1->fSynth1 = max(0.0f,min(6.0f,f1 * tp1->f));
    tp1->fSynth0 = tp1->fSynth1;
    tp1->phSynth = tp1->ph;
    if(mode == synthModeOutput && tp1->dupStereo) {
      return tp1;
    }
  } else if(time == last) {
    if(last < end) {
      TrackPoint *tp0 = getTrackPoint(time);
      tp0->fSynth0 = tp0->fSynth1;
    }
  } else {
    TrackPoint *tp0 = getTrackPoint(time);
    TrackPoint *tp1 = getTrackPoint(time+1);

    if(mode == synthModeOutput) {
      if(tp0->dupStereo && tp1->dupStereo && tp0->dupStereo->owner == tp1->dupStereo->owner) {
        float dp = tp1->ph - tp0->ph;
        float dp0 = 0.5f*h*(tp0->f + tp1->f);
        float dw = canonPI(dp - dp0)/h;
        float dpStereo = tp1->dupStereo->ph - tp0->dupStereo->ph;
        float dp0Stereo = 0.5f*h*(tp0->dupStereo->f + tp1->dupStereo->f);
        float dwStereo = canonPI(dpStereo - dp0Stereo)/h;
        if(dw > .0013f * (tp0->f + tp1->f)) {
          dw = 0;
          dwStereo = 0;
        } else if(dwStereo > .0013f * (tp0->dupStereo->f + tp1->dupStereo->f)) {
          dwStereo = 0;
        }
        float w0 = 0.5f * (tp0->f + tp0->dupStereo->f + dw + dwStereo);
        float w1 = 0.5f * (tp1->f + tp1->dupStereo->f + dw + dwStereo);
        float dwSynth = 0.5f * canonPI(dp - dpStereo) / n;
        if(!(bSplit && time == first)) {
          tp0->fSynth0 = max(0.0f,min(6.0f,f0 * (w0 + dwSynth)));
        }
        if(!(bMerge && time + 1 == last)) {
          tp1->fSynth1 = max(0.0f,min(6.0f,f1 * (w1 + dwSynth)));
        }
      } else {
        float dp = tp1->ph - tp0->ph;
        float dp0 = 0.5f*h*(tp0->f + tp1->f);
        float dw = canonPI(dp - dp0)/h;
        if(dw > .0013f * (tp0->f + tp1->f)) {
          dw = 0;
        }
        if(!(bSplit && time == first)) {
          tp0->fSynth0 = max(0.0f,min(6.0f,f0 * (tp0->f + dw)));
        }
        if(!(bMerge && time + 1 == last)) {
          tp1->fSynth1 = max(0.0f,min(6.0f,f1 * (tp1->f + dw)));
        }
      }
  
      if(!(tp0->bSplit || tp0->bMerge || tp1->bSplit || tp1->bMerge) && jump(tp0,tp1)) {
        tp1->bJump = true;
        if(tp0->dupStereo && tp1->dupStereo) {
          if(tp0->dupStereo->owner == tp1->dupStereo->owner) {
            tp1->bSyncStereo = !jump(tp0->dupStereo,tp1->dupStereo);
          }
        }
      }

      if(!tp0->bSplit) {
        if(tp0->bJump) {
          if(tp0->bSyncStereo) {
            tp0->phSynth = canon2PI(tp0->dupStereo->phSynth + tp0->ph - tp0->dupStereo->ph);
          } else {
            tp0->phSynth = tp0->ph;
          }
        }
      }

      if(!(bMerge && time + 1 == last)) {
        float dw = (tp1->fSynth1 - tp0->fSynth0) / n;
        float w = tp0->fSynth0 + 0.5f * dw;
        float iw = lrintf(w * WScale) / WScale;
        float idw = lrintf(dw * WScale) / WScale;
        tp1->phSynth = canon2PI(tp0->phSynth + n * iw + ((n * (n - 1)) >> 1) * idw);
      }
    } else {
      float dp = tp1->ph - tp0->ph;
      float dp0 = 0.5f*h*(tp0->f + tp1->f);
      float dw = canonPI(dp - dp0)/h;
      if(dw > .0013f * (tp0->f + tp1->f)) {
        dw = 0;
      }
      if(!(bSplit && time == first)) {
        tp0->fSynth0 = max(0.0f,min(6.0f,f0 * (tp0->f + dw)));
        tp0->phSynth = tp0->ph;
      }
      if(!(bMerge && time + 1 == last)) {
        tp1->fSynth1 = max(0.0f,min(6.0f,f1 * (tp1->f + dw)));
        tp1->phSynth = tp1->ph;
      }
    }
  }
  return NULL;
}

void Track :: updateM(const TimeType &time, int mode)
{
  if(mode == synthModeTrial2) {
    if(time == first && time == start) {
      TrackPoint *tp0 = getTrackPoint(time);
      tp0->m = (tp0->m2>0.0f?sqrt(tp0->m2):0.0f);
    }
    if(time < last) {
      TrackPoint *tp1 = getTrackPoint(time+1);
      tp1->m = (tp1->m2>0.0f?sqrt(tp1->m2):0.0f);
    }
  }
}

void Track :: step(const TimeType &time)
{
  if(time > first && time < last) {
    TrackPoint *tp = point[time-first];
    tp->destroy();
    point[time-first] = NULL;
  }
}

void Track :: push_back(TrackPoint *p)
{
  point.push_back(p);
  p->owner = this;
  p->refCount++;
  last++;
  end++;
}

void Track :: endTrack(bool bStitch)
{
  if(bStitch) {
    this->bStitch = true;
  } else {
    end++;
  }
  bEnded = true;
}

void Track :: synth(float *out,
                    const TimeType &time,
                    int n,
                    int mode,
                    int c)
{
  float m0, m1;
  float w0, w1;
  // unused   float dw;
  float ph0, ph1;
  bool bTailStart;
  bool bTailEnd;
  if(time >= end) return;
  if(time < last) {
    TrackPoint *tp1 = getTrackPoint(time+1);
    w1 = tp1->fSynth1;
    m1 = tp1->m;
    ph1 = tp1->phSynth;
    if(bMerge && time + 1 == last) {
      m1 = 0.0f;
    }
    bTailStart = tp1->bJump;
    bTailEnd = tp1->bJump;
  } else {
    bTailStart = false;
    bTailEnd = (last != end);
  }
  if(time >= first) {
    TrackPoint *tp0 = getTrackPoint(time);
    w0 = tp0->fSynth0;
    m0 = tp0->m;
    ph0 = tp0->phSynth;
    if(bSplit && time == first) {
      m0 = 0.0f;
    }
  } else {
    bTailStart = true;
  }

  if(bTailEnd) {
    int fall = min(n,w0==0.0f?384:min(384,(int)lrintf(PI * 4.0f / w0)));
    float dm = m0 / (float)fall;
    float w = w0;
    float *out2 = out;
    float *end = out + fall;
    long iph = lrintf(ph0 * WScale);
    if(iph>=W2PI) iph -= W2PI;
    long iw = lrintf(w * WScale);
    while(out2 != end) {
      if(iw < WPI) {
        long f = (iph>>PhShift)&Ph1;
        long i = iph>>WShift;
        *out2 += m0 * (float)(synthTable1[i] + f * synthTable2[i]);
      }
      out2++;
      m0 -= dm;
      iph += iw;
      iph &= W2PIMask;
    }
  }

  if(bTailStart) {
    int rise = min(n,w1==0.0f?384:min(384,(int)lrintf(PI * 3.0f / w1)));
    float dm = m1 / (float)rise;
    float w = w1;
    out += n;
    float *end = out-rise;
    long iph = lrintf(ph1 * WScale);
    iph &= W2PIMask;
    long iw = lrintf(w * WScale);
    while(out != end) {
      out--;
      m1 -= dm;
      iph -= iw;
      if(iph<0) iph += W2PI;
      if(iw < WPI) {
        long f = (iph>>PhShift)&Ph1;
        long i = iph>>WShift;
        *out += m1 * (float)(synthTable1[i] + f * synthTable2[i]);
      }
    }
  }

  if(!(bTailStart || bTailEnd)) {
    float dw = (w1 - w0) / n;
    float w = w0 + 0.5f * dw;
    float dm = (m1 - m0) / n;
    long iph = lrintf(ph0 * WScale);
    if(iph>=W2PI) iph -= W2PI;
    long iw = lrintf(w * WScale);
    long idw = lrintf(dw * WScale);

    float *end = out + n;
    while(out != end) {
      if(iw < WPI) {
        long f = (iph>>PhShift)&Ph1;
        long i = iph>>WShift;
        *out += m0 * (float)(synthTable1[i] + f * synthTable2[i]);
      }
      iph += iw;
      iw += idw;
      iph &= W2PIMask;
      m0 += dm;
      out++;
    }
  }
}

void Track :: absorb()
{
  for(vector<TrackPoint*>::iterator i = point.begin();
      i != point.end();
      ++i) {
    TrackPoint *tp = (*i);
    tp->absorb();
  }
}

}
