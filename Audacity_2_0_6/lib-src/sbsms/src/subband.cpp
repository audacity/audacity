#include "subband.h"
#include "real.h"
#include "sbsms.h"
#include "utils.h"
#include <algorithm>
using namespace std;

namespace _sbsms_ {

SubBand :: SubBand(SubBand *parent, int band, int channels, SBSMSQuality *quality, bool bSynthesize)
{
  if(band<quality->params.bands-1) {
    sub = new SubBand(this,band+1,channels,quality,bSynthesize);
  } else {
    sub = NULL;
  }
  this->quality = quality;
  this->channels = channels;
  this->parent = parent;
  this->band = band;
  int M = (1<<band);
  int M_MAX = 1<<(quality->params.bands-1);
  this->N = quality->params.N[band];
  int N0 = quality->params.N0[band];
  int N1 = quality->params.N1[band];  
  int N2 = quality->params.N2[band];
  this->res = quality->params.res[band];
  this->resMask = res - 1;
  this->bSynthesize = bSynthesize;
  nGrainsPerFrame = res;
  if(sub) nGrainsPerFrame *= sub->nGrainsPerFrame;
  inputFrameSize = M_MAX*quality->params.H;
  h = inputFrameSize / (M*nGrainsPerFrame);
  nToDrop0 = (quality->getMaxPresamples()/M - N0/2);
  nToDrop1 = (quality->getMaxPresamples()/M - N1/2);
  nToDrop2 = (quality->getMaxPresamples()/M - N2/2);
  nToWriteForGrain = (quality->getMaxPresamples()/M + N2/2);
  nToPrepad1 = N1/2;
  nToPrepad0 = N0/2;
  nReadFromOutputFrame = 0;
  for(int i=0; i<3; i++) {
    nFramesAnalyzed[i] = 0;
  }
  for(int c=0; c<channels; c++) {
    nFramesExtracted[c] = 0;
    nFramesMarked[c] = 0;
    nFramesAssigned[c] = 0;
    nFramesTrialed2[c] = 0;
    nFramesTrialed1[c] = 0;
    nFramesRendered[c] = 0;
    nGrainsMarked[c] = 0;
    nGrainsAssigned[c] = 0;
    nGrainsTrialed2[c] = 0;
    nGrainsTrialed1[c] = 0;
    nGrainsAdvanced[c] = 0;
  }
  nGrainsWritten = 0;
  nFramesAdjusted2 = 0;
  nGrainsAdjusted2 = 0;
  nFramesAdjusted1 = 0;
  nGrainsAdjusted1 = 0;
  nFramesRead = 0;
  totalSizef = 0.0;
  if(sub) {
    samplesSubIn = new SampleBuf(NDownSample/2);
    grainsIn = new GrainBuf(NDownSample, NDownSample/SDownSample, NDownSample, hann);
    downSampledGrainAllocator = new GrainAllocator(NDownSample/2,NDownSample/2,hann);
  }
  if(band >= minTrial1Band) {
    grains[0] = new GrainBuf(N, h, N0, hannpoisson);
  } else {
    grains[0] = NULL;
  }
  if(band >= minTrial2Band) {
    grains[1] = new GrainBuf(N, h, N1, hannpoisson);
  } else {
    grains[1] = NULL;
  }
  grains[2] = new GrainBuf(N, h, N2, hannpoisson);
  for(int c=0; c<channels; c++) {
    if(band >= minTrial1Band) {
      analyzedGrains[0][c] = new GrainBuf(N, h, N0, hannpoisson);
    } else {
      analyzedGrains[0][c] = NULL;
    }
    if(band >= minTrial2Band) {
      analyzedGrains[1][c] = new GrainBuf(N, h, N1, hannpoisson);
    } else {
      analyzedGrains[1][c] = NULL;
    }
    analyzedGrains[2][c] = new GrainBuf(N, h, N2, hannpoisson);
  }
#ifdef MULTITHREADED
  pthread_mutex_init(&dataMutex, NULL);
  for(int i=0; i<3; i++) {
    pthread_mutex_init(&grainMutex[i], NULL);
  }
#endif
  sms = new SMS(sub?sub->sms:NULL,N,band,quality->params.bands-1,h,res,N0,N1,N2,channels,analyzedGrains[2][0]->getWindowFFT());
  nTrial2Latency = sms->getTrial2Latency() / nGrainsPerFrame + 1;
  if(sms->getTrial2Latency() % nGrainsPerFrame) nTrial2Latency++;
  int nAdjust2LatencyGrains = N1/(2*h);
  nAdjust2Latency = nAdjust2LatencyGrains / nGrainsPerFrame + 1;
  if(nAdjust2LatencyGrains % nGrainsPerFrame) nAdjust2Latency++;
  int nAdjust1LatencyGrains = N0/(2*h);
  nAdjust1Latency = nAdjust1LatencyGrains / nGrainsPerFrame + 1;
  if(nAdjust1LatencyGrains % nGrainsPerFrame) nAdjust1Latency++;
  if(sub) nTrial2Latency = max(nTrial2Latency,sub->nTrial2Latency);
  if(sub) nAdjust2Latency = max(nAdjust2Latency,sub->nAdjust2Latency);
  if(sub) nAdjust1Latency = max(nAdjust1Latency,sub->nAdjust1Latency);
  nMarkLatency = 1;
  nAssignLatency = 1;
  nTrial1Latency = 1;
  nRenderLatency = 1;
  if(band==0) {
    SubBand *s = sub;
    while(s) {
      s->nTrial2Latency = nTrial2Latency;
      s->nAdjust2Latency = nAdjust2Latency;
      s->nAdjust1Latency = nAdjust1Latency;
      s = s->sub;
    }
  }
#ifdef MULTITHREADED
  nWriteSlack = 6;
  nAnalyzeSlack = 6;
  nExtractSlack = 6;
  nMarkSlack = 6;
  nAssignSlack = 6;
  nTrial2Slack = 6;
  nAdjust2Slack = 6;
  nTrial1Slack = 6;
  nAdjust1Slack = 6;
  nRenderSlack = 6;
#else
  nWriteSlack = 2;
  nAnalyzeSlack = 2;
  nExtractSlack = 2;
  nMarkSlack = 2;
  nAssignSlack = 2;
  nTrial2Slack = 2;
  nAdjust2Slack = 2;
  nTrial1Slack = 2;
  nAdjust1Slack = 2;
  nRenderSlack = 2;
#endif
  synthRenderer = NULL;
  outMixer = NULL;
  if(bSynthesize) {
    synthRenderer = new SynthRenderer(channels,M*h);
    renderers.push_back(synthRenderer);
    if(sub) {
      samplesSubOut = new SampleBuf(0);
      outMixer = new Mixer(synthRenderer,samplesSubOut);
    } else {
      outMixer = synthRenderer;
    }
  }       
}

SubBand :: ~SubBand() 
{
  for(int i=0; i<3; i++) {
    if(grains[i]) {
      delete grains[i];
    }
    for(int c=0; c<channels; c++) {
      if(analyzedGrains[i][c]) {
        delete analyzedGrains[i][c];
      }
    }
  }
  delete sms;
  if(sub) {
    delete sub;
    delete grainsIn;
    delete samplesSubIn;
    delete downSampledGrainAllocator;
    if(bSynthesize) {
      delete samplesSubOut;
      delete outMixer;
    }
  }
  if(bSynthesize) delete synthRenderer;
}

void SubBand :: addRenderer(SBSMSRenderer *renderer)
{
  if(sub) sub->addRenderer(renderer);
  renderers.push_back(renderer);
}

void SubBand :: removeRenderer(SBSMSRenderer *renderer)
{
  if(sub) sub->removeRenderer(renderer);
  renderers.remove(renderer);
}

void SubBand :: setStretch(float stretch)
{
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  if(!parent) {
    float oFrameSizef = (stretch==0.0f?1.0f:stretch)*(float)inputFrameSize;
    totalSizef += oFrameSizef;
    long oFrameSizei = lrintf(totalSizef);
    totalSizef -= oFrameSizei;
    outputFrameSize.write(oFrameSizei);
  }
  stretchRender.write(stretch);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
  if(sub) sub->setStretch(stretch);
}

void SubBand :: setPitch(float f)
{
  if(sub) sub->setPitch(f);
#ifdef MULTITHREADED
    pthread_mutex_lock(&dataMutex);
#endif
  pitchRender.write(f);
#ifdef MULTITHREADED
    pthread_mutex_unlock(&dataMutex);
#endif    
}

void SubBand :: stepAnalyzeFrame(int i)
{
  if(sub) sub->stepAnalyzeFrame(i);
  nFramesAnalyzed[i]++;
}

void SubBand :: stepExtractFrame(int c)
{
  if(sub) sub->stepExtractFrame(c);
  nFramesExtracted[c]++;
}

void SubBand :: stepMarkFrame(int c)
{
  if(sub) sub->stepMarkFrame(c);
  nFramesMarked[c]++;
}

void SubBand :: stepAssignFrame(int c)
{
  if(sub) sub->stepAssignFrame(c);
  nFramesAssigned[c]++;
}

void SubBand :: stepTrial2Frame(int c)
{
  if(sub) sub->stepTrial2Frame(c);
  nFramesTrialed2[c]++;
}

void SubBand :: stepAdjust2Frame()
{
  if(sub) sub->stepAdjust2Frame();
  nFramesAdjusted2++;
}
void SubBand :: stepTrial1Frame(int c)
{
  if(sub) sub->stepTrial1Frame(c);
  nFramesTrialed1[c]++;
}

void SubBand :: stepAdjust1Frame()
{
  if(sub) sub->stepAdjust1Frame();
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  stretchRender.advance(1);
  pitchRender.advance(1);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
  nFramesAdjusted1++;
}

void SubBand :: stepRenderFrame(int c)
{
  if(sub) sub->stepRenderFrame(c);
  nFramesRendered[c]++;
}

void SubBand :: stepReadFrame()
{
  if(sub) sub->stepReadFrame();
  nFramesRead++;
}

bool SubBand :: writeInit()
{
  long n = getFramesAtFront(0);
  n = min(n,getFramesAtFront(1));
  n = min(n,getFramesAtFront(2));
  return (n <= nWriteSlack);
}

long SubBand :: readInit()
{
  long n = nFramesRendered[0];
  for(int c=1; c<channels; c++) {
    n = max(0L,min(1L,min(n,nFramesRendered[c]-nFramesRead)));
  }
  if(sub) n = min(n,sub->readInit());
  return n;
}

long SubBand :: analyzeInit(int i, bool bSet, long n)
{
  if(!parent) {
    n = getFramesAtFront(i);
    for(int c=0; c<channels; c++) {
      n = max(0L,min(1L,min(n,nAnalyzeSlack-(long)(nFramesAnalyzed[i]-nFramesExtracted[c]))));
    }
  }
  if(bSet) {
    nGrainsToAnalyze[i] = n * nGrainsPerFrame;
    if(sub) {
      sub->analyzeInit(i,bSet,n);
    }
  }
  return n;
}

long SubBand :: extractInit(int c, bool bSet)
{
  long n;
  if(sub) n = res*sub->extractInit(c,bSet);
  if(!sub) {
    n = max(0L,min(1L,nExtractSlack+nMarkLatency-(long)(nFramesExtracted[c]-nFramesMarked[c])));
    for(int i=0; i<3; i++) {
      n = max(0L,min(1L,min(n,(long)(nFramesAnalyzed[i]-nFramesExtracted[c]))));
    }
  }
  if(bSet) {
    nGrainsToExtract[c] = n;
  }
  return n;
}

long SubBand :: markInit(int c, bool bSet)
{
  long n;
  if(sub) n = res*sub->markInit(c,bSet);
  if(!sub) n = max(0L,min(1L,min((long)(nFramesExtracted[c]-nFramesMarked[c])-nMarkLatency,
                                 nMarkSlack+nAssignLatency-(long)(nFramesMarked[c]-nFramesAssigned[c]))));
  if(bSet) {
    nGrainsToMark[c] = n;
  }
  return n;
}

long SubBand :: assignInit(int c, bool bSet)
{
  long n;
  if(sub) {
    n = res * sub->assignInit(c,bSet);
  } else {
    n = max(0L,min(1L,min((long)(nFramesMarked[c]-nFramesAssigned[c])-nAssignLatency,
                          nAssignSlack+nTrial2Latency-(long)(nFramesAssigned[c]-nFramesTrialed2[c]))));
  }
  if(bSet) {
    nGrainsToAdvance[c] = n;
    nGrainsToAssign[c] = n;
    if(n) {
      if(nFramesAssigned[c]==0) {
        sms->start(0,c);
      }      
    }
  }
  return n;
}

long SubBand :: trial2Init(int c, bool bSet)
{
  long n;
  if(sub) {
    n = res * sub->trial2Init(c,bSet);
  } else {
    n = max(0L,min(1L,min((long)(nFramesAssigned[c]-nFramesTrialed2[c])-nTrial2Latency,
                          nTrial2Slack+nAdjust2Latency-(long)(nFramesTrialed2[c]-nFramesAdjusted2))));
  }
  if(bSet) {
    nGrainsToTrial2[c] = n;
    nGrainsTrialed2[c] = 0;
  }
  return n;
}

long SubBand :: adjust2Init(bool bSet)
{
  long n;
  if(sub) {
    n = res * sub->adjust2Init(bSet);
  } else {
    n = 1;
    for(int c=0; c<channels; c++) {
      n = min(n,(long)(nFramesTrialed2[c]-nFramesAdjusted2-nAdjust2Latency));
      n = min(n,nAdjust2Slack+nTrial1Latency-(long)(nFramesAdjusted2-nFramesTrialed1[c]));
    }
    n = max(0L,n);
  }
  if(bSet) {
    nGrainsToAdjust2 = n;
    nGrainsAdjusted2 = 0;
  }
  return n;
}

long SubBand :: trial1Init(int c, bool bSet)
{
  long n;
  if(sub) {
    n = res * sub->trial1Init(c,bSet);
  } else {
    n = max(0L,min(1L,min((long)(nFramesAdjusted2-nFramesTrialed1[c])-nTrial1Latency,
                          nTrial1Slack+nAdjust1Latency-(long)(nFramesTrialed1[c]-nFramesAdjusted1))));
  }
  if(bSet) {
    nGrainsToTrial1[c] = n;
    nGrainsTrialed1[c] = 0;
  }
  return n;
}

long SubBand :: adjust1Init(bool bSet)
{
  long n;
  if(sub) {
    n = res * sub->adjust1Init(bSet);
  } else {
    n = 1;
    for(int c=0; c<channels; c++) {
      n = min(n,(long)(nFramesTrialed1[c]-nFramesAdjusted1-nAdjust1Latency));
      n = min(n,nAdjust1Slack+nRenderLatency-(long)(nFramesAdjusted1-nFramesRendered[c]));
    }
    n = max(0L,n);
  }
  if(bSet) {
    nGrainsToAdjust1 = n;
    nGrainsAdjusted1 = 0;
  }
  return n;
}

long SubBand :: renderInit(int c, bool bSet) 
{
  long n;
  if(sub) {
    n = res * sub->renderInit(c,bSet);
  } else {
    n = max(0L,min(1L,min((long)(nFramesAdjusted1-nFramesRendered[c])-nRenderLatency,
                          nRenderSlack-(long)(nFramesRendered[c]-nFramesRead))));
  }
  if(bSet) {
    nGrainsRendered[c] = 0;
    nGrainsToRender[c] = n;
  }
  return n;
}

void SubBand :: analyze(int i)
{
  if(sub) sub->analyze(i);
  if(grains[i]) {
    vector<grain*> gV;
#ifdef MULTITHREADED
    pthread_mutex_lock(&grainMutex[i]);
#endif
    for(int k=grains[i]->readPos;k<grains[i]->readPos+nGrainsToAnalyze[i];k++) {
      grain *g = grains[i]->read(k);
      gV.push_back(g);
    }
#ifdef MULTITHREADED
    pthread_mutex_unlock(&grainMutex[i]);
#endif

    for(int k=0;k<nGrainsToAnalyze[i];k++) {
      gV[k]->analyze();
    }

#ifdef MULTITHREADED
    pthread_mutex_lock(&grainMutex[i]);
#endif
    for(int k=0;k<nGrainsToAnalyze[i];k++) {
      for(int c=0; c<channels; c++) {
        analyzedGrains[i][c]->write(gV[k]);
      }
    }
    grains[i]->advance(nGrainsToAnalyze[i]);
#ifdef MULTITHREADED
    pthread_mutex_unlock(&grainMutex[i]);
#endif
  }
}

void SubBand :: extract(int c)
{
  if(sub) sub->extract(c);
  vector<grain*> gV[3];

  for(int i=0; i<3; i++) {
    if(grains[i]) {
#ifdef MULTITHREADED
      pthread_mutex_lock(&grainMutex[i]);
#endif    
      for(int k=analyzedGrains[i][c]->readPos;
          k<analyzedGrains[i][c]->readPos+nGrainsToExtract[c];
          k++) {
        grain *g = analyzedGrains[i][c]->read(k);
        gV[i].push_back(g);
      }
#ifdef MULTITHREADED
      pthread_mutex_unlock(&grainMutex[i]);
#endif
    }
  }

  for(int k=0;k<nGrainsToExtract[c];k++) {
    grain *g0 = (grains[0]?gV[0][k]:NULL);
    grain *g1 = (grains[1]?gV[1][k]:NULL);
    grain *g2 = gV[2][k];
    sms->add(g0,g1,g2,c);
  }

  for(int i=0; i<3; i++) {
    if(grains[i]) {
#ifdef MULTITHREADED
      pthread_mutex_lock(&grainMutex[i]);
#endif
      analyzedGrains[i][c]->advance(nGrainsToExtract[c]);
#ifdef MULTITHREADED
      pthread_mutex_unlock(&grainMutex[i]);
#endif
    }
  }
}

void SubBand :: mark(int c)
{
  long ntodo = parent?1:nGrainsToMark[c];
  long ndone = 0;
  while(ndone<ntodo) {  
    sms->mark(nGrainsMarked[c],c);
    if(nGrainsMarked[c]&resMask || res==1) {
      if(sub) sub->mark(c);
    }
    ndone++;
    nGrainsMarked[c]++;
  }
}

void SubBand :: assign(int c) 
{
  for(int ndone=0; ndone<nGrainsToAssign[c]; ndone++) {
    assignStart(c);
    bool bCont = true;
    while(bCont) {
      assignInit(c);
      assignFind(c);
      bCont = assignConnect(c);
    }
    assignStep(c);
    splitMerge(c);
  }
}

void SubBand :: assignStart(int c)
{
  if(sub && !(nGrainsAssigned[c]&resMask)) sub->assignStart(c);
  sms->assignStart(nGrainsAssigned[c],c);
}

void SubBand :: assignInit(int c)
{
  if(sub) sub->assignInit(c);
  sms->assignInit(nGrainsAssigned[c],c);
}

void SubBand :: assignFind(int c)
{
  if(sub) sub->assignFind(c);
  sms->assignFind(nGrainsAssigned[c],c);
}

bool SubBand :: assignConnect(int c)
{
  bool bCont = false;
  if(sub) {
    if(sub->assignConnect(c)) {
      bCont = true;
    }
  }  
  if(sms->assignConnect(nGrainsAssigned[c],c,false)) {
    bCont = true;
  }
  return bCont;
}

void SubBand :: assignStep(int c)
{
  sms->assignConnect(nGrainsAssigned[c],c,true);
  if(sub && !((nGrainsAssigned[c]+1)&resMask)) {
    sub->assignStep(c);
  }
  sms->start(nGrainsAssigned[c]+1,c);
}

void SubBand :: splitMerge(int c)
{
  nGrainsAssigned[c]++;
  if(sub && !(nGrainsAssigned[c]&resMask)) {
    sub->splitMerge(c);
  }
  sms->splitMerge(c);
}

void SubBand :: advance(int c)
{
  long ntodo = parent?1:nGrainsToAdvance[c];
  long ndone = 0;
  while(ndone<ntodo) {
    if(sub && !(nGrainsAdvanced[c]&resMask)) {
      sub->advance(c);
    }
    sms->advance(c);
    nGrainsMarked[c]--;
    nGrainsAssigned[c]--;
    nGrainsAdvanced[c]++;
    ndone++;
  }
}

void SubBand :: trial2(int c)
{
  for(int i=0; i<nGrainsToTrial2[c]; i++) {  
    trial2Start(c);
    trial2Trial(c);
    trial2End(c);
  }
}

void SubBand :: trial2Start(int c)
{
  if(!(nGrainsTrialed2[c]&resMask)) {
    if(sub) sub->trial2Start(c);
    sms->trial2Start(c);
  }
}

void SubBand :: trial2Trial(int c)
{
  if(sub && !(nGrainsTrialed2[c]&resMask)) {
    sub->trial2Trial(c);
  }
  sms->trial2(c);
}

void SubBand :: trial2End(int c)
{
  nGrainsTrialed2[c]++;
  if(!(nGrainsTrialed2[c]&resMask)) {
    if(sub) sub->trial2End(c);
    sms->trial2End(c);
  }
}

void SubBand :: adjust2()
{
  long ntodo = parent?1:nGrainsToAdjust2;
  long ndone = 0;
  while(ndone<ntodo) {  
    if(!(nGrainsAdjusted2&resMask)) {
      if(sub) sub->adjust2();
    }
    sms->adjust2();
    ndone++;
    nGrainsAdjusted2++;
  }
}

void SubBand :: trial1(int c)
{
  for(int i=0; i<nGrainsToTrial1[c]; i++) {  
    trial1Start(c);
    trial1Trial(c);
    trial1End(c);
  }
}

void SubBand :: trial1Start(int c)
{
  if(!(nGrainsTrialed1[c]&resMask)) {
    if(sub) sub->trial1Start(c);
    sms->trial1Start(c);
  }
}

void SubBand :: trial1Trial(int c)
{
  if(sub && !(nGrainsTrialed1[c]&resMask)) {
    sub->trial1Trial(c);
  }
  sms->trial1(c);
}

void SubBand :: trial1End(int c)
{
  nGrainsTrialed1[c]++;
  if(!(nGrainsTrialed1[c]&resMask)) {
    if(sub) sub->trial1End(c);
    sms->trial1End(c);
  }
}

void SubBand :: adjust1()
{
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  float stretch = stretchRender.read();
  float f0 = pitchRender.read(pitchRender.readPos);
  float f1;
  if(pitchRender.nReadable()>=2) {
    f1 = pitchRender.read(pitchRender.readPos+1);
  } else {
    f1 = f0;
  }
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
  long ntodo = parent?1:nGrainsToAdjust1;
  long ndone = 0;
  float df = (f1-f0)/(float)nGrainsToAdjust1;
  while(ndone<ntodo) {  
    if(!(nGrainsAdjusted1&resMask)) {
      if(sub) sub->adjust1();
    }
    sms->adjust1(stretch,f0+nGrainsAdjusted1*df,f0+(nGrainsAdjusted1+1)*df);
    ndone++;
    nGrainsAdjusted1++;
  }
}

void SubBand :: readSubSamples()
{
  if(sub) sub->readSubSamples();
  if(sub) {
    audio fromSub[subBufSize];
    long nFromSub = 0;
    do {
      nFromSub = sub->outMixer->read(fromSub,subBufSize);
      samplesSubOut->write(fromSub, nFromSub);
    } while(nFromSub>0);
  }
}

long SubBand :: read(audio *buf, long n) 
{
  long nRead = 0;
  long nToRead = n;
  readSubSamples();
  while(nToRead && nRead < n && outputFrameSize.nReadable()) {
    long nToReadFromOutputFrame = outputFrameSize.read();
    nToRead = min(n-nRead,nToReadFromOutputFrame-nReadFromOutputFrame);
    nToRead = outMixer->read(buf+nRead, nToRead);
    nReadFromOutputFrame += nToRead;
    nRead += nToRead;
    if(nReadFromOutputFrame == nToReadFromOutputFrame) {
      nReadFromOutputFrame = 0;
      outputFrameSize.advance(1);
      stepReadFrame();
    }
  }
  return nRead;
}

long SubBand :: renderSynchronous() 
{
  for(list<SBSMSRenderer*>::iterator i = renderers.begin(); i != renderers.end(); ++i) {
    SBSMSRenderer *renderer = *i;
    renderer->startFrame();
  }
  for(int c=0; c<channels; c++) {    
    renderInit(c,true);
    render(c);
    stepRenderFrame(c);
  }
  for(list<SBSMSRenderer*>::iterator i = renderers.begin(); i != renderers.end(); ++i) {
    SBSMSRenderer *renderer = *i;
    renderer->endFrame();
  }
  long samples = outputFrameSize.read();
  outputFrameSize.advance(1);
  stepReadFrame();
  return samples;
}

void SubBand :: render(int c)
{
  long ntodo = parent?1:nGrainsToRender[c];
  long ndone = 0;
  long nRenderedTotal = 0;

  while(ndone<ntodo) {
    if(sub && !(nGrainsRendered[c]&resMask)) {
      sub->render(c);
    }
    sms->render(c,renderers);
    nGrainsRendered[c]++;
    ndone++;
  }
}

void SubBand :: renderComplete(const SampleCountType &samples)
{
  for(list<SBSMSRenderer*>::iterator i = renderers.begin(); i != renderers.end(); ++i) {
    SBSMSRenderer *renderer = *i;
    renderer->end(samples);
  }
}

long SubBand :: write(audio *inBuf, long n, float stretch, float pitch)
{
  long nWritten = 0;

  while(nWritten<n) {
    long nToWrite = min(nToWriteForGrain,n-nWritten);
    if(nToDrop2) {
      nToWrite = min(nToDrop2,nToWrite);
      nToDrop2 -= nToWrite;
      nToDrop1 -= nToWrite;
      nToDrop0 -= nToWrite;
    } else {
      if(nToDrop1) {
        nToWrite = min(nToDrop1,nToWrite);
        nToDrop1 -= nToWrite;
        nToDrop0 -= nToWrite;
      } else {
        if(nToDrop0) {
          nToWrite = min(nToDrop0,nToWrite);
        } else if(nToPrepad0) {
          nToWrite = min(nToPrepad0,nToWrite);
        }
        if(nToPrepad1) {
          nToWrite = min(nToPrepad1,nToWrite);
          sms->prepad1(inBuf+nWritten, nToWrite);
          nToPrepad1 -= nToWrite;
        }
        if(nToDrop0) {
          nToDrop0 -= nToWrite;
        } else {
          if(nToPrepad0) {
            sms->prepad0(inBuf+nWritten, nToWrite);
            nToPrepad0 -= nToWrite;
          }
#ifdef MULTITHREADED
          pthread_mutex_lock(&grainMutex[0]);
#endif      
          if(grains[0]) {
            grains[0]->write(inBuf+nWritten, nToWrite);
          }
#ifdef MULTITHREADED
          pthread_mutex_unlock(&grainMutex[0]);
#endif
        }
#ifdef MULTITHREADED
        pthread_mutex_lock(&grainMutex[1]);
#endif      
        if(grains[1]) {
          grains[1]->write(inBuf+nWritten, nToWrite);
        }
#ifdef MULTITHREADED
        pthread_mutex_unlock(&grainMutex[1]);
#endif
      }
#ifdef MULTITHREADED
      pthread_mutex_lock(&grainMutex[2]);
#endif      
      grains[2]->write(inBuf+nWritten, nToWrite);
#ifdef MULTITHREADED
      pthread_mutex_unlock(&grainMutex[2]);
#endif
    }
    nWritten += nToWrite;
    nToWriteForGrain -= nToWrite;
    if(nToWriteForGrain == 0) {
      nToWriteForGrain = h;
      if(!parent) {
        if(nGrainsWritten == 0) {
          setStretch(stretch);
          setPitch(pitch);
        }
        nGrainsWritten++;
        if(nGrainsWritten == nGrainsPerFrame) {
          nGrainsWritten = 0;
        }
      }
    }
  }

  if(sub) {
    grainsIn->write(inBuf, n);
    long nGrainsRead = 0;
    for(int k=grainsIn->readPos;k<grainsIn->writePos;k++) {
      grain *g = grainsIn->read(k); g->analyze();
      grain *gdown = downSampledGrainAllocator->create();
      g->downsample(gdown);
      samplesSubIn->write(gdown, hSub);
      downSampledGrainAllocator->forget(gdown);
      nGrainsRead++;
    }
    grainsIn->advance(nGrainsRead);  
    long nWriteToSub = samplesSubIn->nReadable();
    audio *subBuf = samplesSubIn->getReadBuf();
    nWriteToSub = sub->write(subBuf,nWriteToSub,stretch,pitch);
    samplesSubIn->advance(nWriteToSub);
  }
  return n;
}

void SubBand :: process(bool bRender)
{
  for(int i=0; i<3; i++) {
    if(analyzeInit(i,true)) {
      analyze(i);
      stepAnalyzeFrame(i);
    }
  }

  for(int c=0; c<channels; c++) {
    if(extractInit(c,true)) {
      extract(c);
      stepExtractFrame(c);
    }

    if(markInit(c,true)) {
      mark(c);
      stepMarkFrame(c);
    }
    
    if(assignInit(c,true)) {
      assign(c);
      advance(c);
      stepAssignFrame(c);
    }

    if(trial2Init(c,true)) {
      trial2(c);
      stepTrial2Frame(c);
    }

    if(adjust2Init(true)) {
      adjust2();
      stepAdjust2Frame();
    }

    if(trial1Init(c,true)) {
      trial1(c);
      stepTrial1Frame(c);
    }

    if(adjust1Init(true)) {
      adjust1();
      stepAdjust1Frame();
    }

    if(bRender) {
      if(renderInit(c,true)) {
        render(c);
        stepRenderFrame(c);
      }
    }
  }
}

long SubBand :: getFramesAtFront(int i)
{
  long n = 65536;
#ifdef MULTITHREADED
  pthread_mutex_lock(&grainMutex[i]);
#endif
  if(grains[i]) {
    n = grains[i]->nReadable() / nGrainsPerFrame;
  }
#ifdef MULTITHREADED
  pthread_mutex_unlock(&grainMutex[i]);
#endif
  if(sub) n = min(n,sub->getFramesAtFront(i));
  return n;
}

long SubBand :: getInputFrameSize()
{
  return inputFrameSize;
}

}
