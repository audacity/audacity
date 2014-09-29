// -*- mode: c++ -*-
#ifndef SMS_H
#define SMS_H

#include "config.h"
#ifdef MULTITHREADED
#include "pthread.h"
#endif
#include "sbsms.h"
#include "track.h"
#include "grain.h"
#include "buffer.h"
#include <queue>
#include <list>
using namespace std;

namespace _sbsms_ {

enum {
  minTrial2Band = 1,
  minTrial1Band = 2
};

class SynthRenderer : public SBSMSRenderer, public SampleBufBase {
 public:
  SynthRenderer(int channels, int h);
  ~SynthRenderer();
  void startTime(int c, const TimeType &time, int n);
  void render(int c, SBSMSTrack *t);
  void endTime(int c);
  long read(audio *out, long n);

 protected:
  int channels;
  float *synthBuf[2];
  int synthBufLength[2];
  ArrayRingBuffer<float> *sines[2];
  TimeType time[2];
  int n[2];
#ifdef MULTITHREADED
  pthread_mutex_t bufferMutex;
#endif
};

class SMS {
 public:
  SMS(SMS *lo, int N, int band, int bandMax, int h, int res, int N0, int N1, int N2, int channels, audio *peak2);
  ~SMS();
  void render(int c, list<SBSMSRenderer*> &renderers);
  void add(grain *g0, grain *g1, grain *g2, int c);
  void assignStart(long offset, int c);
  void assignInit(long offset, int c);
  void assignFind(long offset, int c);
  bool assignConnect(long offset, int c, bool bLastDitch);
  void start(long offset, int c);
  void splitMerge(int c);
  void mark(long offset, int c);
  void advance(int c);
  void trial2(int c);
  void trial2Start(int c);
  void trial2End(int c);
  void adjust2();
  void trial1(int c);
  void trial1Start(int c);
  void trial1End(int c);
  void adjust1(float stretch, float pitch0, float pitch1);
  void adjustInit(ArrayRingBuffer<float> **trialRingBuf,
                  GrainBuf *trialGrainBuf);
  void adjust(GrainBuf *trialGrainBuf,
              queue<float*> *magQueue,
              int minCutSep,
              float **_mag1,
              float **_dmag1,
              audio **x1,
              const TimeType &time,
              Slice **slice);
  void prepad1(audio *buf, long n);
  void prepad0(audio *buf, long n);
  int getTrial2Latency();

 protected:
  void connect(TrackPoint *tp0, TrackPoint *tp1, int ilo, int c);
  void mark(long offset, long offsetlo, int c);
  TrackPoint *nearestForward(TrackPoint **begin, TrackPoint *tp0, float *minCost2, float maxCost2, float maxDF2, float dMCoeff2, float dNCoeff2 = 0.0f);
  TrackPoint *nearestReverse(TrackPoint **begin, TrackPoint *tp0, float *minCost2, float maxCost2, float maxDF2, float dMCoeff2, float dNCoeff2 = 0.0f);
  TrackPoint *nearestForward2(TrackPoint **begin, TrackPoint *tp0, float *minCost2, float maxCost2, float maxDF2, float dMCoeff2, float dNCoeff2 = 0.0f);
  TrackPoint *nearestReverse2(TrackPoint **begin, TrackPoint *tp0, float *minCost2, float maxCost2, float maxDF2, float dMCoeff2, float dNCoeff2 = 0.0f);
  float interp2(int k, int ko1, float kf);
  float findExtremum(float *mag, float *mag2, int k, float *y);
  void calcmags(float *mag, audio *x);
  int findCut(float *dmag, int k, int maxK);
  Track *createTrack(int c, TrackPoint *tp, const TimeType &time, bool bStitch);
  void returnTrackIndex(int c, Track *t);

  list<TrackPoint*> ended[2];
  list<TrackPoint*> started[2];
  int minTrackSize; 
  int peakWidth0;
  int peakWidth1;
  int peakWidth2;
  int minCutSep1;
  int minCutSep2;
  int minK;
  int maxK;
  float peakThresh;
  float maxCost2;
  float maxDF;
  float dMCoeff2;
  float dNCoeff2;
  float maxDFSplitMerge;
  float maxCost2SplitMerge;
  float dMCoeff2SplitMerge;
  float maxCost2Match;
  float maxDFMatch;
  float dMCoeff2Match;
  float maxCost2Stereo;
  float maxDFStereo;
  float dMCoeff2Stereo;
  float maxFMatchM;
  float minFMatchL;
  float minFLo;
  float maxFHi;
  float minFMid;
  float maxFMid;
  int kStart;
  int kEnd;
  int kLo;
  int kHi;
  float mNorm;
  float localFavorRatio;
  queue<Slice*> adjust2SliceQueue[2];
  queue<Slice*> adjust1SliceQueue[2];
  RingBuffer<Slice*> sliceBuffer[2];
  Slice* sliceM0[2];
  Slice* sliceL0[2];
  Slice* sliceH0[2];
  Slice* sliceM1[2];
  Slice* sliceL1[2];
  Slice* sliceM2[2];
  Slice* sliceH1[2];
  audio* x10[2];
  audio* x11[2];
  float* dmag1[2];
  float* mag11[2];
  audio* x00[2];
  audio* x01[2];
  float* dmag0[2];
  float* mag01[2];
  float *mag2[2];
  audio* x2[2];
  float* dec2[2];
  float *peak20;
  float *peak2N;
  int N;
  int Nover2;
  SMS *lo;
  SMS *hi;
  queue<TrackIndexType> trackIndex[2];
  queue<float*> mag1Queue[2];
  queue<float*> mag0Queue[2];
  float *trial2Buf[2];
  ArrayRingBuffer<float> *trial2RingBuf[2];
  GrainBuf *trial2GrainBuf;
  float *trial1Buf[2];
  ArrayRingBuffer<float> *trial1RingBuf[2];
  GrainBuf *trial1GrainBuf;
  list<Track*> assignTracks[2];
  list<Track*> renderTracks[2];
  TimeType addtime[2];
  TimeType assigntime[2];
  TimeType trial2time[2];
  TimeType adjust2time;
  TimeType trial1time[2];
  TimeType adjust1time;
  TimeType synthtime[2];
  queue<int> nRender[2];
  double h2cum;
  int channels;
  long res;
  long resMask;
  int h;
  float M;
  double h1;
  int band;  
#ifdef MULTITHREADED
  pthread_mutex_t sliceMutex[2];
  pthread_mutex_t magMutex[2];
  pthread_mutex_t renderMutex[2];
  pthread_mutex_t trial2Mutex[2];
  pthread_mutex_t trial1Mutex[2];
  pthread_mutex_t trackMutex[2];
#endif
  bool bAssignDone[2];
};

}

#endif
