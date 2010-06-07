#ifndef TRACK_H
#define TRACK_H

#include "buffer.h"
#include "trackpoint.h"
#include "config.h"
#ifdef MULTITHREADED
#include "pthread.h"
#endif
#include <stack>
#include <vector>
using namespace std;

namespace _sbsms_ {

class sms;
class renderer;

class track {
  friend class renderer;
 public:
  void endTrack(bool bTail);
  bool isEnded();
  bool isDone();
  void startTrack(trackpoint *p, bool bTail);
  void push_back(trackpoint *p);
  void push_back_tpoint(tpoint *p);
  long size();
  void synth(SampleBuf *out,
             long writePos,
             int c,
             long synthtime,
             int steps,
             real fScale0,
             real fScale1,
             real mScale);
  trackpoint *getTrackPoint(long time);
  bool isStart(long synthtime);
  bool isEnd(long synthtime);
  trackpoint *back();

  vector<tpoint*> point;

  unsigned short index;
  bool bEnd;
  track *descendant;
  track *precursor;
  long tailEnd;
  long tailStart;
  long currtime;
  real rise;
  real fall;
  long start;
  long end;
  real m_p;
  real m_pDescendant;
  int res;
  sms *owner;

 protected:
  friend class TrackAllocator;
  track(track *precursor,sms *owner,int res);
  ~track();
};

class TrackAllocator {
 public:
  TrackAllocator(bool bManageIndex);
  TrackAllocator(bool bManageIndex, unsigned short maxtrackindex);
  void init();
  int size();
  ~TrackAllocator();

  track *getTrack(unsigned short index);
  track *create(track *precursor,sms *owner, int res, unsigned short index);
  track *create(track *precursor,sms *owner, int res);
  void destroy(track *t);

 protected:
#ifdef MULTITHREADED
  pthread_mutex_t taMutex;
#endif
  long gIdCount;
  stack<unsigned short> gTrackIndex;
  vector<track*> gTrack;
  bool bManageIndex;

};

}

#endif
