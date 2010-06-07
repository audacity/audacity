#ifndef SUBBAND_H
#define SUBBAND_H

#include "real.h"
#include "buffer.h"
#include "audio.h"
#include "sms.h"
#include <stdio.h>
#include "config.h"
#ifdef MULTITHREADED
#include "pthread.h"
#endif

namespace _sbsms_ {

class renderer;

class subband {
  friend class renderer;
 public:
  subband(subband *parent, unsigned short M, int channels, sbsms_quality *quality, int latency, bool bPreAnalysis, TrackAllocator *ta, PeakAllocator *pa);
  ~subband();

  long write(audio *buf, long n, real a, real ratio);
  void process();
  long read(audio *buf, real *ratio0, real *ratio1);
  long n_readable();
  bool isframe_readable();
  void writingComplete();
  long readFromFile(FILE *fp, real a, real ratio);
  long synth();
  long getFramesWrittenToFile();
  long writeToFile(FILE *fp);
  void init();
  void reset();
  void seek(long framePos);
  long getSamplesQueued();
  long getFramesQueued();
  long getFramesAtFront();
  long getFramesAtBack();
  long getLastInputFrameSize();
  long getFramePos();
  void write_(audio *buf, long n, real a, real ratio);

  void writeFramePositionsToFile(FILE *fp);
  long addInit(bool);
  void addTrackPoints();
  long markInit(bool, int c);
  void markDuplicates(int c);
  long assignInit(bool,int c);
  long assignTrackPoints(int c);
  void startNewTracks(int c);
  void advanceTrackPoints(int c);
  long synthInit(bool);
  void synthTracks();
  long readInit(bool);
  long writeTracksToFile(FILE *fp);
  void readTrackPointsFromFile(FILE *fp);
  long writeTrackPointsToFile(FILE *fp); 
  void stepAddFrame();
  void stepMarkFrame(int c);
  void stepAssignFrame(int c);
  void stepSynthFrame();
  void stepReadFrame();
  void readSubSamples();
  void setA(real a);
  void setAMod(real a);
  void setAForH(real a);
  void setH(real ratio);
  void setF(real f);
  void setFrameSize(int inputSize, real a, real ratio);
  void setRatio(real ratio);
  void setFramesInFile(long frames);
  long zeroPad();
  long zeroPad_();

  long preAnalyze(audio *buf, long n, real a, real ratio);
  void preAnalyzeComplete();

  FILE *fp;

  long getFramesAssigned();
  long getFramesMarked();

  bool isWriteReady();
  bool isAddReady();
  bool isMarkReady();
  bool isAssignReady();
  bool isSynthReady();
#ifdef MULTITHREADED
  pthread_mutex_t dataMutex;
  pthread_mutex_t bufferMutex;
#endif

  long nLatency;
  long nLatencyOriginal;

 protected:
  long read(audio *buf, long n);
  void calculateA(long kstart, long kend);
  real calculateOnset(grain *g1, grain *g2);

  TrackAllocator *ta;
  PeakAllocator *pa;

  RingBuffer<real> aPreAnalysis;
  RingBuffer<real> aMod;
  RingBuffer<real> aSynth;
  RingBuffer<real> aForH;
  RingBuffer<real> fSynth;
  RingBuffer<int> inputFrameSize;
  RingBuffer<int> outputFrameSize;
  RingBuffer<real> frameRatio;
  RingBuffer<int> frameBytes;
  RingBuffer<real> onset;
  grain *gPrev;

  real getOnset(long k);
  int lastInputFrameSize;
  int lastOutputFrameSize;
  real samplesQueued;
  real lastFrameA;
  real lastFrameRatio;
  real totalSizef;
  bool bPreAnalyze;
  sbsms_quality *quality;
  int channels;
  int N;
  int h;
  unsigned short M;
  int s;
  int res;
  int nGrainsPerFrame;
  int resTotal;
  long nDropped;
  long nToDrop;

  long nTrackPointsToAdd;
  long nTrackPointsToMark[2];
  long nTrackPointsToAssign[2];
  long nTrackPointsToAdvance[2];
  long nTrackPointsToSynth;

  long nTrackPointsMarked[2];
  long nTrackPointsAssigned[2];
  long nTrackPointsStarted[2];
  long nTrackPointsAdvanced[2];
  long nTrackPointsSynthed;
  long nTrackPointsRead;
  long nFramesSkipped;
  long nFramesWritten;
  long nFramesAdded;
  long nFramesMarked[2];
  long nFramesAssigned[2];
  long nFramesRead;
  long nFramesSynthed;
  long nFramesInFile;
  long nTrackPointsWritten;

  bool bWritingComplete;
  subband *parent;
  subband *sub;
  GrainBuf *in0,*in1,*in2;
  SampleBufBase *outMixer;
  sms *smser;
  SampleBuf *subIn;
  SampleBuf *subOut;
  GrainBuf *in;
  GrainBuf *inPre;
  
  grain *x1[2];
  grain *x2[2];

};

}

#endif
