#include "config.h"
#include "sbsms.h"
#include "real.h"
#include "subband.h"
#ifdef MULTITHREADED
#include <pthread.h>
#endif
#include <stdlib.h>
#include <string.h>
#include <algorithm>
using namespace std;

namespace _sbsms_ {

const SBSMSQualityParams SBSMSQualityStandard = {
  8,3,
  {512,512,384,384,384,384,384,384,0,0},
  {168,144,128,96,64,36,24,14,0,0},
  {384,288,256,168,128,84,52,28,0,0},
  {512,448,360,288,192,128,84,44,0,0},
  {1,1,2,1,1,2,1,1,0,0}
};


SBSMSQuality :: SBSMSQuality(const SBSMSQualityParams *params)
{
  this->params = *params;
}

long SBSMSQuality :: getFrameSize()
{
  return (1<<(params.bands-1)) * params.H;
}

long SBSMSQuality :: getMaxPresamples()
{
  long prepad = 0;
  for(int i=0; i<params.bands; i++) {
    prepad = max(prepad,(long)((1<<i) * (params.N2[i]>>1)));
  }
  prepad += ((1<<(params.bands-1)) - 1) * (NDownSample>>1);
  long framesize = (1<<(params.bands-1)) * params.H;
  long frames = prepad / framesize;
  if(prepad%framesize) frames++;
  frames++;
  prepad = frames * framesize;
  return prepad;
}

#ifdef MULTITHREADED
class ThreadInterface;
#endif

class SBSMSImp {
public:
  SBSMSImp(int channels, SBSMSQuality *quality, bool bSynthesize);
  ~SBSMSImp();
  inline long read(SBSMSInterface *iface, audio *buf, long n);
  inline void addRenderer(SBSMSRenderer *renderer);
  inline void removeRenderer(SBSMSRenderer *renderer);
  inline long renderFrame(SBSMSInterface *iface);
  SubBand *top;  
#ifdef MULTITHREADED
  friend class ThreadInterface;
  ThreadInterface *threadInterface;
#endif
  friend class SBSMS;
protected:
  inline void reset();
  float getInputTime(SBSMSInterface *iface);
  long write(SBSMSInterface *);
  FILE *fpIn;  
  FILE *fpOut;
  SBSMSError error;
  long nPrepad;
  long nPrepadDone;
  long nPresamplesDone;
  SampleCountType nSamplesInputed;
  SampleCountType nSamplesOutputed;
  int channels;
  SBSMSQuality *quality;
  audio *ina;
};

#ifdef MULTITHREADED

struct channel_thread_data {
  int c;
  ThreadInterface *threadInterface;
};

struct analyze_thread_data {
  int i;
  ThreadInterface *threadInterface;
};

class ThreadInterface {
public:
  friend class SBSMSImp;
  ThreadInterface(SBSMSImp *sbsms, bool bSynthesize);  
  ~ThreadInterface();
  void signalReadWrite();
  void signalAnalyze();
  void signalExtract(int c);
  void signalMark(int c);
  void signalAssign(int c);
  void signalTrial2(int c);
  void signalAdjust2();
  void signalTrial1(int c);
  void signalAdjust1();
  void signalRender(int c);
  void waitReadWrite();
  void waitAnalyze(int i);
  void waitExtract(int c);
  void waitAssign(int c);
  void waitTrial2(int c);
  void waitAdjust2();
  void waitTrial1(int c);
  void waitAdjust1();
  void waitRender(int c);
  SubBand *top;
  int channels;
  pthread_mutex_t readWriteMutex;
  pthread_cond_t readWriteCond;
  pthread_t analyzeThread[3];
  pthread_mutex_t analyzeMutex[3];
  pthread_cond_t analyzeCond[3];
  pthread_t extractThread[2];
  pthread_mutex_t extractMutex[2];
  pthread_cond_t extractCond[2];
  pthread_t assignThread[2];
  pthread_mutex_t assignMutex[2];
  pthread_cond_t assignCond[2];
  pthread_t trial2Thread[2];
  pthread_mutex_t trial2Mutex[2];
  pthread_cond_t trial2Cond[2];
  pthread_t adjust2Thread;
  pthread_mutex_t adjust2Mutex;
  pthread_cond_t adjust2Cond;
  pthread_t trial1Thread[2];
  pthread_mutex_t trial1Mutex[2];
  pthread_cond_t trial1Cond[2];
  pthread_t adjust1Thread;
  pthread_mutex_t adjust1Mutex;
  pthread_cond_t adjust1Cond;
  bool bRenderThread;
  pthread_t renderThread[2];
  pthread_mutex_t renderMutex[2];
  pthread_cond_t renderCond[2];
  channel_thread_data channelData[2];
  analyze_thread_data analyzeData[3];
  bool bActive;
};

void *analyzeThreadCB(void *data) {
  analyze_thread_data *analyzeData = (analyze_thread_data*)data;
  ThreadInterface *threadInterface = analyzeData->threadInterface;
  SubBand *top = threadInterface->top;
  int i = analyzeData->i;
  int channels = threadInterface->channels;
  while(threadInterface->bActive) {
    threadInterface->waitAnalyze(i);
    if(top->analyzeInit(i,true)) {
      top->analyze(i);
      top->stepAnalyzeFrame(i);
      threadInterface->signalReadWrite();
      for(int c=0; c<channels; c++) {
        threadInterface->signalExtract(c);
      }
    }
  }
  pthread_exit(NULL);
  return NULL;
}

void *extractThreadCB(void *data) {
  channel_thread_data *channelData = (channel_thread_data*)data;
  ThreadInterface *threadInterface = channelData->threadInterface;
  SubBand *top = threadInterface->top;
  int c = channelData->c;
  while(threadInterface->bActive) {
    threadInterface->waitExtract(c);
    if(top->extractInit(c,true)) {
      top->extract(c);
      top->stepExtractFrame(c);
      threadInterface->signalAnalyze();
      threadInterface->signalMark(c);        
    }
  }
  pthread_exit(NULL);
  return NULL;
}

void *assignThreadCB(void *data) {
  channel_thread_data *channelData = (channel_thread_data*)data;
  ThreadInterface *threadInterface = channelData->threadInterface;
  SubBand *top = threadInterface->top;
  int c = channelData->c;
  while(threadInterface->bActive) {
    threadInterface->waitAssign(c);
    if(top->markInit(c,true)) {
      top->mark(c);
      top->stepMarkFrame(c);      
      threadInterface->signalExtract(c);
    }
    if(top->assignInit(c,true)) {
      top->assign(c);
      top->advance(c);
      top->stepAssignFrame(c);
      threadInterface->signalTrial2(c);
    }
  }
  pthread_exit(NULL);
  return NULL;
}

void *trial2ThreadCB(void *data) {
  channel_thread_data *channelData = (channel_thread_data*)data;
  ThreadInterface *threadInterface = channelData->threadInterface;
  SubBand *top = threadInterface->top;
  int c = channelData->c;
  while(threadInterface->bActive) {
    threadInterface->waitTrial2(c);
    if(top->trial2Init(c,true)) {
      top->trial2(c);
      top->stepTrial2Frame(c);
      threadInterface->signalAssign(c);
      threadInterface->signalAdjust2();
    }
  }
  pthread_exit(NULL);
  return NULL;
}

void *adjust2ThreadCB(void *data) {
  ThreadInterface *threadInterface = (ThreadInterface*)data;
  int channels = threadInterface->channels;
  SubBand *top = threadInterface->top;
  while(threadInterface->bActive) {
    threadInterface->waitAdjust2();
    if(top->adjust2Init(true)) {
      top->adjust2();
      top->stepAdjust2Frame();
      for(int c=0; c<channels; c++) {
        threadInterface->signalTrial2(c);
      }
      for(int c=0; c<channels; c++) {
        threadInterface->signalTrial1(c);
      }
    }
  }
  pthread_exit(NULL);
  return NULL;
}

void *trial1ThreadCB(void *data) {
  channel_thread_data *channelData = (channel_thread_data*)data;
  ThreadInterface *threadInterface = channelData->threadInterface;
  SubBand *top = threadInterface->top;
  int c = channelData->c;
  while(threadInterface->bActive) {
    threadInterface->waitTrial1(c);
    if(top->trial1Init(c,true)) {
      top->trial1(c);
      top->stepTrial1Frame(c);
      threadInterface->signalAdjust2();
      threadInterface->signalAdjust1();
    }
  }
  pthread_exit(NULL);
  return NULL;
}

void *adjust1ThreadCB(void *data) {
  ThreadInterface *threadInterface = (ThreadInterface*)data;
  int channels = threadInterface->channels;
  SubBand *top = threadInterface->top;
  while(threadInterface->bActive) {
    threadInterface->waitAdjust1();
    if(top->adjust1Init(true)) {
      top->adjust1();
      top->stepAdjust1Frame();
      for(int c=0; c<channels; c++) {
        threadInterface->signalTrial1(c);
      }
      if(threadInterface->bRenderThread) {
        for(int c=0; c<channels; c++) {
          threadInterface->signalRender(c);
        }
      } else {
        threadInterface->signalReadWrite();
      }
    }
  }
  pthread_exit(NULL);
  return NULL;
}

void *renderThreadCB(void *data) {
  channel_thread_data *channelData = (channel_thread_data*)data;
  ThreadInterface *threadInterface = channelData->threadInterface;
  SubBand *top = threadInterface->top;
  int c = channelData->c;
  while(threadInterface->bActive) {
    threadInterface->waitRender(c);
    if(top->renderInit(c,true)) {
      top->render(c);
      top->stepRenderFrame(c);
      threadInterface->signalAdjust1();
      threadInterface->signalReadWrite();
    }
  }
  pthread_exit(NULL);
  return NULL;
}

ThreadInterface :: ThreadInterface(SBSMSImp *sbsms, bool bSynthesize) 
{
  this->top = sbsms->top;
  this->channels = sbsms->channels;
  bActive = true;
  pthread_cond_init(&readWriteCond, NULL);
  pthread_mutex_init(&readWriteMutex, NULL);
  if(bSynthesize) {
    bRenderThread = true;
  } else {
    bRenderThread = false;
  }
  for(int i=0; i<3; i++) {
    analyzeData[i].i = i;
    analyzeData[i].threadInterface = this;
    pthread_cond_init(&analyzeCond[i], NULL);
    pthread_mutex_init(&analyzeMutex[i], NULL);
  }
  for(int c=0; c<channels; c++) {
    channelData[c].c = c;
    channelData[c].threadInterface = this;
    pthread_cond_init(&extractCond[c], NULL);
    pthread_mutex_init(&extractMutex[c], NULL);
    pthread_cond_init(&assignCond[c], NULL);    
    pthread_mutex_init(&assignMutex[c], NULL);
    pthread_cond_init(&trial2Cond[c], NULL);    
    pthread_mutex_init(&trial2Mutex[c], NULL);
    pthread_cond_init(&trial1Cond[c], NULL);    
    pthread_mutex_init(&trial1Mutex[c], NULL);
    if(bRenderThread) {
      pthread_cond_init(&renderCond[c], NULL);
      pthread_mutex_init(&renderMutex[c], NULL);
    }
  }
  for(int i=0; i<3; i++) {
    pthread_create(&analyzeThread[i], NULL, analyzeThreadCB, (void*)&analyzeData[i]);
  }
  for(int c=0; c<channels; c++) {
    pthread_create(&extractThread[c], NULL, extractThreadCB, (void*)&channelData[c]);
    pthread_create(&assignThread[c], NULL, assignThreadCB, (void*)&channelData[c]);
    pthread_create(&trial2Thread[c], NULL, trial2ThreadCB, (void*)&channelData[c]);
    pthread_create(&trial1Thread[c], NULL, trial1ThreadCB, (void*)&channelData[c]);
    if(bRenderThread) {
      pthread_create(&renderThread[c], NULL, renderThreadCB, (void*)&channelData[c]);
    }
  }
  pthread_cond_init(&adjust2Cond, NULL);    
  pthread_mutex_init(&adjust2Mutex, NULL);
  pthread_create(&adjust2Thread, NULL, adjust2ThreadCB, this);
  pthread_cond_init(&adjust1Cond, NULL);    
  pthread_mutex_init(&adjust1Mutex, NULL);
  pthread_create(&adjust1Thread, NULL, adjust1ThreadCB, this);
}

ThreadInterface :: ~ThreadInterface() 
{
  bActive = false;
  for(int i=0; i<3; i++) {
    pthread_mutex_lock(&analyzeMutex[i]);
    pthread_cond_broadcast(&analyzeCond[i]);
    pthread_mutex_unlock(&analyzeMutex[i]);
    pthread_join(analyzeThread[i],NULL);
  }
  for(int c=0; c<channels; c++) {
    pthread_mutex_lock(&extractMutex[c]);
    pthread_cond_broadcast(&extractCond[c]);
    pthread_mutex_unlock(&extractMutex[c]);
    pthread_join(extractThread[c],NULL);
    pthread_mutex_lock(&assignMutex[c]);
    pthread_cond_broadcast(&assignCond[c]);
    pthread_mutex_unlock(&assignMutex[c]);
    pthread_join(assignThread[c],NULL);
    pthread_mutex_lock(&trial2Mutex[c]);
    pthread_cond_broadcast(&trial2Cond[c]);
    pthread_mutex_unlock(&trial2Mutex[c]);
    pthread_join(trial2Thread[c],NULL);
    pthread_mutex_lock(&trial1Mutex[c]);
    pthread_cond_broadcast(&trial1Cond[c]);
    pthread_mutex_unlock(&trial1Mutex[c]);
    pthread_join(trial1Thread[c],NULL);
    if(bRenderThread) {
      pthread_mutex_lock(&renderMutex[c]);
      pthread_cond_broadcast(&renderCond[c]);
      pthread_mutex_unlock(&renderMutex[c]);
      pthread_join(renderThread[c],NULL);
    }
  }
  pthread_mutex_lock(&adjust2Mutex);
  pthread_cond_broadcast(&adjust2Cond);
  pthread_mutex_unlock(&adjust2Mutex);
  pthread_join(adjust2Thread,NULL);
  pthread_mutex_lock(&adjust1Mutex);
  pthread_cond_broadcast(&adjust1Cond);
  pthread_mutex_unlock(&adjust1Mutex);
  pthread_join(adjust1Thread,NULL);
}

void ThreadInterface :: signalReadWrite() 
{
  pthread_mutex_lock(&readWriteMutex);
  bool bReady;
  if(bRenderThread) {
    bReady = (top->writeInit() || top->readInit());
  } else {
    if(top->writeInit()) {
      bReady = true;  
    } else {
      bReady = true;
      for(int c=0; c<channels; c++) {
        if(!top->renderInit(c,false)) {
          bReady = false;
          break;
        }
      }
    }
  }
  if(bReady) {
    pthread_cond_broadcast(&readWriteCond);
  }
  pthread_mutex_unlock(&readWriteMutex);
}

void ThreadInterface :: signalAnalyze() 
{
  for(int i=0; i<3; i++) {
    pthread_mutex_lock(&analyzeMutex[i]);
    if(top->analyzeInit(i,false)) {
      pthread_cond_broadcast(&analyzeCond[i]);
    }
    pthread_mutex_unlock(&analyzeMutex[i]);
  }
}

void ThreadInterface :: signalExtract(int c) {
  pthread_mutex_lock(&extractMutex[c]);
  if(top->extractInit(c,false)) {
    pthread_cond_broadcast(&extractCond[c]);
  }
  pthread_mutex_unlock(&extractMutex[c]);
}

void ThreadInterface :: signalMark(int c) {
  pthread_mutex_lock(&assignMutex[c]);
  if(top->markInit(c,false)) {
    pthread_cond_broadcast(&assignCond[c]);
  }
  pthread_mutex_unlock(&assignMutex[c]);
}

void ThreadInterface :: signalAssign(int c) {
  pthread_mutex_lock(&assignMutex[c]);
  if(top->assignInit(c,false)) {
    pthread_cond_broadcast(&assignCond[c]);
  }
  pthread_mutex_unlock(&assignMutex[c]);
}

void ThreadInterface :: signalTrial2(int c) {
  pthread_mutex_lock(&trial2Mutex[c]);
  if(top->trial2Init(c,false)) {
    pthread_cond_broadcast(&trial2Cond[c]);
  }
  pthread_mutex_unlock(&trial2Mutex[c]);
}

void ThreadInterface :: signalAdjust2() {
  pthread_mutex_lock(&adjust2Mutex);
  if(top->adjust2Init(false)) {
    pthread_cond_broadcast(&adjust2Cond);
  }
  pthread_mutex_unlock(&adjust2Mutex);
}

void ThreadInterface :: signalTrial1(int c) {
  pthread_mutex_lock(&trial1Mutex[c]);
  if(top->trial1Init(c,false)) {
    pthread_cond_broadcast(&trial1Cond[c]);
  }
  pthread_mutex_unlock(&trial1Mutex[c]);
}

void ThreadInterface :: signalAdjust1() {
  pthread_mutex_lock(&adjust1Mutex);
  if(top->adjust1Init(false)) {
    pthread_cond_broadcast(&adjust1Cond);
  }
  pthread_mutex_unlock(&adjust1Mutex);
}

void ThreadInterface :: signalRender(int c) {
  pthread_mutex_lock(&renderMutex[c]);
  if(top->renderInit(c,false)) {
    pthread_cond_broadcast(&renderCond[c]);
  }
  pthread_mutex_unlock(&renderMutex[c]);
}

void ThreadInterface :: waitReadWrite() {
  pthread_mutex_lock(&readWriteMutex);
  bool bReady;
  if(bRenderThread) {
    bReady = (top->writeInit() || top->readInit());
  } else {
    if(top->writeInit()) {
      bReady = true;  
    } else {
      bReady = true;
      for(int c=0; c<channels; c++) {
        if(!top->renderInit(c,false)) {
          bReady = false;
          break;
        }
      }
    }
  }
  if(!bReady) {
    pthread_cond_wait(&readWriteCond,&readWriteMutex);
  }
  pthread_mutex_unlock(&readWriteMutex);
}

void ThreadInterface :: waitAnalyze(int i) {
  pthread_mutex_lock(&analyzeMutex[i]);
  if(!top->analyzeInit(i,false)) {
    pthread_cond_wait(&analyzeCond[i],&analyzeMutex[i]);
  }
  pthread_mutex_unlock(&analyzeMutex[i]);
}

void ThreadInterface :: waitExtract(int c) {
  pthread_mutex_lock(&extractMutex[c]);
  if(!top->extractInit(c,false)) {
    pthread_cond_wait(&extractCond[c],&extractMutex[c]);
  }
  pthread_mutex_unlock(&extractMutex[c]);
}

void ThreadInterface :: waitAssign(int c) {
  pthread_mutex_lock(&assignMutex[c]);
  if(!top->markInit(c,false) && !top->assignInit(c,false)) {
    pthread_cond_wait(&assignCond[c],&assignMutex[c]);
  }
  pthread_mutex_unlock(&assignMutex[c]);
}

void ThreadInterface :: waitTrial2(int c) {
  pthread_mutex_lock(&trial2Mutex[c]);
  if(!top->trial2Init(c,false)) {
    pthread_cond_wait(&trial2Cond[c],&trial2Mutex[c]);
  }
  pthread_mutex_unlock(&trial2Mutex[c]);
}

void ThreadInterface :: waitAdjust2() {
  pthread_mutex_lock(&adjust2Mutex);
  if(!top->adjust2Init(false)) {
    pthread_cond_wait(&adjust2Cond,&adjust2Mutex);
  }
  pthread_mutex_unlock(&adjust2Mutex);
}

void ThreadInterface :: waitTrial1(int c) {
  pthread_mutex_lock(&trial1Mutex[c]);
  if(!top->trial1Init(c,false)) {
    pthread_cond_wait(&trial1Cond[c],&trial1Mutex[c]);
  }
  pthread_mutex_unlock(&trial1Mutex[c]);
}

void ThreadInterface :: waitAdjust1() {
  pthread_mutex_lock(&adjust1Mutex);
  if(!top->adjust1Init(false)) {
    pthread_cond_wait(&adjust1Cond,&adjust1Mutex);
  }
  pthread_mutex_unlock(&adjust1Mutex);
}

void ThreadInterface :: waitRender(int c) {
  pthread_mutex_lock(&renderMutex[c]);
  if(!top->renderInit(c,false)) {
    pthread_cond_wait(&renderCond[c],&renderMutex[c]);
  }
  pthread_mutex_unlock(&renderMutex[c]);
}

#endif

void SBSMSImp :: reset()
{
  nSamplesInputed = 0;
  nSamplesOutputed = 0;
  nPrepadDone = 0;
  nPresamplesDone = 0;
}

SBSMS :: SBSMS(int channels, SBSMSQuality *quality, bool bSynthesize)
{ imp = new SBSMSImp(channels,quality,bSynthesize); }
SBSMSImp :: SBSMSImp(int channels, SBSMSQuality *quality, bool bSynthesize)
{
  this->channels = channels;
  this->quality = new SBSMSQuality(&quality->params);
  error = SBSMSErrorNone;
  top = new SubBand(NULL,0,channels,quality,bSynthesize);
  ina = (audio*)malloc(quality->getFrameSize()*sizeof(audio));
  nPrepad = quality->getMaxPresamples();
  reset();
#ifdef MULTITHREADED
  threadInterface = new ThreadInterface(this,bSynthesize);
#endif
}

SBSMS :: ~SBSMS() { delete imp; }
SBSMSImp :: ~SBSMSImp()
{
#ifdef MULTITHREADED
  if(threadInterface) delete threadInterface;
#endif
  if(top) delete top;
  if(ina) free(ina);
  if(quality) delete quality;
}

void SBSMS :: addRenderer(SBSMSRenderer *renderer) { imp->addRenderer(renderer); }
void SBSMSImp :: addRenderer(SBSMSRenderer *renderer)
{
  top->addRenderer(renderer);
}

void SBSMS :: removeRenderer(SBSMSRenderer *renderer) { imp->removeRenderer(renderer); }
void SBSMSImp :: removeRenderer(SBSMSRenderer *renderer)
{
  top->removeRenderer(renderer);
}

SBSMSError SBSMS :: getError()
{
  return imp->error;
}

float SBSMSImp :: getInputTime(SBSMSInterface *iface)
{
  return (float)nSamplesInputed / (float)iface->getSamplesToInput();
}

long SBSMSImp :: write(SBSMSInterface *iface)
{
  long nWrite = 0;

  float t = getInputTime(iface);
  float t1 = (float)(nSamplesInputed + quality->getFrameSize()) / (float)iface->getSamplesToInput();
  float stretch = iface->getMeanStretch(t,t1);
  float pitch = iface->getPitch(t);

  long nPresamples = iface->getPresamples();
  if(nPrepadDone < nPrepad - nPresamples) {
    stretch = 1.0f;
    nWrite = min(quality->getFrameSize(),nPrepad - nPresamples - nPrepadDone);
    memset(ina,0,nWrite*sizeof(audio));
    nPrepadDone += nWrite;
  } else if(nPresamplesDone < nPresamples) {
    stretch = 1.0f;
    nWrite = min(quality->getFrameSize(),nPresamples - nPresamplesDone);
    nWrite = iface->samples(ina,nWrite);
    if(nWrite == 0) {
      nWrite = quality->getFrameSize();
      memset(ina,0,nWrite*sizeof(audio));
    } else {
      nPresamplesDone += nWrite;
    }
  } else {
    nWrite = iface->samples(ina,quality->getFrameSize());
    nSamplesInputed += nWrite;
    if(nWrite == 0) {
      nWrite = quality->getFrameSize();
      memset(ina,0,nWrite*sizeof(audio));
    }
  }
  nWrite = top->write(ina, nWrite, stretch, pitch);
  return nWrite;
}

long SBSMS :: read(SBSMSInterface *iface, audio *buf, long n) { return imp->read(iface,buf,n); }
long SBSMSImp :: read(SBSMSInterface *iface, audio *buf, long n)
{
  long nReadTotal = 0;
  while(nReadTotal < n) {
    long nRead;
    nRead = n - nReadTotal;
    nRead = top->read(buf+nReadTotal,nRead);
    nReadTotal += nRead;
    if(nRead) {
#ifdef MULTITHREADED
      if(threadInterface->bRenderThread) {
        for(int c=0; c<channels; c++) {
          threadInterface->signalRender(c);
        }
      }
#endif
    } else {
#ifdef MULTITHREADED
      threadInterface->waitReadWrite();
#endif
      if(top->writeInit()) {
        write(iface);
#ifdef MULTITHREADED
        threadInterface->signalAnalyze();
#endif
      }
    }
#ifdef MULTITHREADED
    if(!threadInterface->bRenderThread) {
      for(int c=0; c<channels; c++) {     
        threadInterface->signalRender(c);
      }
    }
#else
    top->process(true);
#endif
    nSamplesOutputed += nRead;
  }
  return nReadTotal;
}

long SBSMS :: renderFrame(SBSMSInterface *iface) { return imp->renderFrame(iface); }
long SBSMSImp :: renderFrame(SBSMSInterface *iface)
{
  long nRendered = 0;
  while(!nRendered) {
    bool bReady = true;
    for(int c=0; c<channels; c++) {
      if(!top->renderInit(c,false)) {
        bReady = false;
        break;
      }
    }
    if(bReady) {
      nRendered = top->renderSynchronous();
    }
    if(nRendered) {
#ifdef MULTITHREADED
      threadInterface->signalAdjust1();
#endif
    } else {
#ifdef MULTITHREADED
      threadInterface->waitReadWrite();  
#endif      
      if(top->writeInit()) {
        write(iface);
      }
      
#ifdef MULTITHREADED
      threadInterface->signalAnalyze();
#endif
    }
#ifdef MULTITHREADED
#else
    top->process(false);
#endif
    if(nSamplesOutputed >= iface->getSamplesToOutput()) {
      top->renderComplete(iface->getSamplesToOutput());
    }
    nSamplesOutputed += nRendered;
  }
  return nRendered;
}

long SBSMS :: getInputFrameSize()
{
  return imp->top->getInputFrameSize();
}

}
