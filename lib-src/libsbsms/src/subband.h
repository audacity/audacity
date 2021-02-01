// -*- mode: c++ -*-
#ifndef SUBBAND_H
#define SUBBAND_H

#include "real.h"
#include "buffer.h"
#include "sms.h"
#include <stdio.h>
#include "config.h"
#ifdef MULTITHREADED
#include "pthread.h"
#endif

namespace _sbsms_ {

enum {
  NDownSample = 256,
  SDownSample = 4,
  subBufSize = 512,
  hSub = NDownSample/(2*SDownSample)
};

class SubBand {
 public:
  SubBand(SubBand *parent, int band, int channels, SBSMSQuality *quality, bool bSynthesize);
  ~SubBand();

  long write(audio *buf, long n, float stretch, float pitch);
  long read(audio *buf, long n);
  long getInputFrameSize();

  bool writeInit();
  long analyzeInit(int,bool,long n=0);
  long extractInit(int,bool);
  long markInit(int,bool);
  long assignInit(int,bool);
  long trial2Init(int,bool);
  long adjust2Init(bool);
  long trial1Init(int,bool);
  long adjust1Init(bool);
  long renderInit(int,bool);
  long writeFromFileInit(int,bool);
  long readInit();

  void analyze(int);
  void extract(int);
  void mark(int);
  void assign(int);
  void trial2(int);
  void trial2Start(int);
  void trial2Trial(int);
  void trial2End(int);
  void adjust2();
  void trial1(int);
  void trial1Start(int);
  void trial1Trial(int);
  void trial1End(int);
  void adjust1();
  void advance(int);
  void render(int);

  void assignStart(int c);
  void assignInit(int c);
  void assignFind(int c);
  bool assignConnect(int c);
  void assignStep(int c);
  void splitMerge(int c);
  void addRenderer(SBSMSRenderer *);
  void removeRenderer(SBSMSRenderer *);
  long renderSynchronous();
  void renderComplete(const SampleCountType &samples);
  void process(bool);

  void stepAnalyzeFrame(int);
  void stepExtractFrame(int);
  void stepMarkFrame(int);
  void stepAssignFrame(int);
  void stepTrial2Frame(int);
  void stepAdjust2Frame();
  void stepTrial1Frame(int);
  void stepAdjust1Frame();
  void stepRenderFrame(int);
  void stepReadFrame();

#ifdef MULTITHREADED
  pthread_mutex_t dataMutex;
  pthread_mutex_t grainMutex[3];
#endif
  friend class SBSMSImp;

 protected:
  void resetNoRecurse();
  
  long getFramesAtFront(int);
  void readSubSamples();
  void setStretch(float stretch);
  void setPitch(float pitch);

  int nMarkLatency;
  int nAssignLatency;
  int nTrial2Latency;
  int nAdjust2Latency;
  int nTrial1Latency;
  int nAdjust1Latency;
  int nRenderLatency;
  int nWriteSlack;
  int nExtractSlack;
  int nAnalyzeSlack;
  int nMarkSlack;
  int nAssignSlack;
  int nTrial2Slack;
  int nAdjust2Slack;
  int nTrial1Slack;
  int nAdjust1Slack;
  int nRenderSlack;
  list<SBSMSRenderer*> renderers;
  RingBuffer<float> stretchRender;
  RingBuffer<float> pitchRender;
  int inputFrameSize;
  RingBuffer<int> outputFrameSize;
  float totalSizef;
  SBSMSQuality *quality;
  int channels;
  int N;
  int h;
  int band;
  long nReadFromOutputFrame;
  long nToWriteForGrain;
  long res;
  long resMask;
  long nGrainsPerFrame;
  long nToDrop0;
  long nToDrop1;
  long nToDrop2;
  long nToPrepad1;
  long nToPrepad0;
  bool bSynthesize;

  long nGrainsToAnalyze[3];
  long nGrainsToExtract[2];
  long nGrainsToMark[2];
  long nGrainsToAssign[2];
  long nGrainsToAdvance[2];
  long nGrainsToTrial2[2];
  long nGrainsToAdjust2;
  long nGrainsToTrial1[2];
  long nGrainsToAdjust1;
  long nGrainsToRender[2];
  long nGrainsWritten;
  long nGrainsMarked[2];
  long nGrainsAssigned[2];
  long nGrainsTrialed2[2];
  long nGrainsAdjusted2;
  long nGrainsTrialed1[2];
  long nGrainsAdjusted1;
  long nGrainsAdvanced[2];
  long nGrainsRendered[2];
  long nGrainsRead;

  long nFramesAnalyzed[3];
  long nFramesExtracted[2];
  long nFramesMarked[2];
  long nFramesAssigned[2];
  long nFramesTrialed2[2];
  long nFramesAdjusted2;
  long nFramesTrialed1[2];
  long nFramesAdjusted1;
  long nFramesRendered[2];
  long nFramesRead;

  SubBand *parent;
  SubBand *sub;
  SampleBufBase *outMixer;
  SynthRenderer *synthRenderer;
  SMS *sms;
  SampleBuf *samplesSubIn;
  SampleBuf *samplesSubOut;
  GrainBuf *grains[3];
  GrainBuf *analyzedGrains[3][2];
  GrainBuf *grainsIn;
  GrainAllocator *downSampledGrainAllocator;
};

}

#endif
