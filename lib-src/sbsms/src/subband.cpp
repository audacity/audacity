#include "subband.h"
#include "real.h"
#include "sbsms.h"
#include "utils.h"
#include <algorithm>
using namespace std;

namespace _sbsms_ {

#define SUB_BUF_SIZE 4096

bool subband :: isWriteReady()
{
  return (getFramesAtFront() <= 8);
}

bool subband :: isAddReady()
{
  return addInit(false)?true:false;
}

bool subband :: isMarkReady()
{
  bool bReady = false;
  for(int c=0;c<channels;c++) {
    if(markInit(false,c)) 
      bReady = true;
  }
  return bReady;
}

bool subband :: isAssignReady()
{
  bool bReady = false;
  for(int c=0;c<channels;c++) {
    if(assignInit(false,c)) 
      bReady = true;
  }
  return bReady;
}

bool subband :: isSynthReady()
{
  return synthInit(false)?true:false;
}

subband :: subband(subband *parent, unsigned short M, int channels, sbsms_quality *quality, int latency, bool bPreAnalyze, TrackAllocator *ta, PeakAllocator *pa)
{
  this->quality = quality;
  this->ta = ta;
  this->pa = pa;
  int k = ilog2(M);
  this->channels = channels;
  this->parent = parent;
  this->N = quality->N[k];
  this->M = M;
  this->s = quality->S[k];
  this->res = quality->res[k];
  real pad = quality->pad[k];
  if(M<quality->M_MAX) 
    sub = new subband(this,M*2,channels,quality,latency,bPreAnalyze,ta,pa);
  else 
    sub = NULL;
  int Nlo = sub?quality->N[k+1]:0;
  
  nGrainsPerFrame = res;
  if(sub) nGrainsPerFrame *= sub->nGrainsPerFrame;
  
  if(sub) resTotal = 2*sub->resTotal;
  else resTotal = 1;

  nLatencyOriginal = latency;
  gPrev = NULL;

  init();

  // sms 
  in0 = new GrainBuf(N, h, pad*quality->P[k]);
  in1 = new GrainBuf(N, h, pad);
  in2 = new GrainBuf(N, h, pad*quality->Q[k]);

  // synthesize
  smser = new sms(N,(short)M,(short)quality->M_MAX,res,latency,quality->P[k],quality->Q[k],pad,Nlo,channels,ta,pa);

  if(!parent && bPreAnalyze) {
    this->bPreAnalyze = true;
    inPre = new GrainBuf(N, h, 2);
    x1[0] = grain :: create(N,pad);
    x1[1] = grain :: create(N,pad);
    x2[0] = grain :: create(N,pad);
    x2[1] = grain :: create(N,pad);
  } else {
    this->bPreAnalyze = false;
    inPre = NULL;
  }

  if(sub) {
    subOut = new SampleBuf(0);

    // receive
    in = new GrainBuf(N, N/s, 1);

    // samples to sub
    subIn = new SampleBuf(N/2);

    // output samples
    outMixer = new Mixer(smser,subOut);
  } else {
    outMixer = smser;
  }

#ifdef MULTITHREADED
  pthread_mutex_init(&dataMutex, NULL);
  pthread_mutex_init(&bufferMutex, NULL);
#endif
  
}

long subband :: getFramePos()
{
  return nFramesRead;  
}

void subband :: seek(long framePos) {
  if(sub) sub->seek(framePos);
  nFramesWritten = framePos;
  nFramesAdded = framePos;
  nFramesMarked[0] = framePos;
  nFramesMarked[1] = framePos;
  nFramesAssigned[0] = framePos;
  nFramesAssigned[1] = framePos;
  nFramesSynthed = framePos;
  nFramesRead = framePos;
}

void subband :: reset() {
  if(sub) sub->reset();
  init();
  inputFrameSize.clear();
  outputFrameSize.clear();
  aForH.clear();
  aSynth.clear();
  fSynth.clear();
  frameRatio.clear();
  in0->clear();
  in1->clear();
  in2->clear();
  if(sub) {
    in->clear();
    subIn->clear();
    subOut->clear();
  }
  smser->reset();
}

void subband :: init()
{
  int ssms = (N*nGrainsPerFrame)/(resTotal*quality->H[2]);
  h = N/ssms;
  lastInputFrameSize = h*nGrainsPerFrame;
  lastFrameA = 1.0f;
  lastFrameRatio = 1.0f;
  nLatency = nLatencyOriginal;
  nToDrop = (quality->N[quality->bands-1]/quality->H[2]*nGrainsPerFrame-ssms)/2;
  nDropped = 0;
  nFramesSkipped = 0;
  nFramesWritten = 0;
  nFramesAdded = 0;
  nFramesMarked[0] = 0;
  nFramesMarked[1] = 0;
  nFramesAssigned[0] = 0;
  nFramesAssigned[1] = 0;
  nFramesSynthed = 0;
  nFramesRead = 0;
  nTrackPointsWritten = 0;
  nTrackPointsMarked[0] = 0;
  nTrackPointsMarked[1] = 0;
  nTrackPointsAssigned[0] = 0;
  nTrackPointsAssigned[1] = 0;
  nTrackPointsStarted[0] = 0;
  nTrackPointsStarted[1] = 0;
  nTrackPointsAdvanced[0] = 0;
  nTrackPointsAdvanced[1] = 0;

  samplesQueued = 0;

  bWritingComplete = false;
  
  totalSizef = 0.0;
}

subband :: ~subband() 
{
  delete in0;
  delete in1;
  delete in2;
  delete smser;

  if(inPre) {
    delete inPre;
    grain ::destroy(x1[0]);
    grain ::destroy(x1[1]);
    grain ::destroy(x2[0]);
    grain ::destroy(x2[1]);
  }
  if(sub) {
    delete sub;
    delete in;
    delete subIn;
    delete subOut;
    delete outMixer;
  }
}

void subband :: setFrameSize(int iFrameSize, real a, real ratio)
{
  real oFrameSizef = a*(real)iFrameSize;
  totalSizef += oFrameSizef;
  long oFrameSizei = round2int(totalSizef);
  totalSizef -= oFrameSizei;
  inputFrameSize.write(iFrameSize);
  outputFrameSize.write(oFrameSizei);
  lastInputFrameSize = iFrameSize;
  lastOutputFrameSize = oFrameSizei;
  samplesQueued += oFrameSizei * ratio;
  //samplesQueued += oFrameSizei;
}

void subband :: setH(real ratio)
{
  real a = aForH.read(aForH.readPos);
  if(a > 4.0)
    h = quality->H[0];
  else if(a > 2.0 && a <= 4.0)
    h = quality->H[1];
  else if(a >= 0.5 && a <= 2.0)
    h = quality->H[2];
  else if(a >= 0.25 && a < 0.5)
    h = quality->H[3];
  else
    h = quality->H[4];

  h = (h*resTotal)/nGrainsPerFrame;
  in0->h = h;
  in1->h = h;
  in2->h = h;
  if(inPre) inPre->h = h;
  if(!parent) 
    setFrameSize(h*nGrainsPerFrame,a,ratio);
  aForH.advance(1);
}

void subband :: setAForH(real a)
{
  aForH.write(a);
  if(sub) sub->setAForH(a);
}

void subband :: setA(real a)
{
  aSynth.write(a);
  if(sub) sub->setA(a);
  lastFrameA = a;
}

void subband :: setF(real f)
{
  fSynth.write(f);
  if(sub) sub->setF(f);
}

void subband :: setAMod(real a)
{
  aMod.write(a);
}

void subband :: setRatio(real ratio)
{
  frameRatio.write(ratio);
  lastFrameRatio = ratio;
}

void subband :: write_(audio *inBuf, long n, real a, real ratio) 
{
  long nwritten = 0;
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif

  while(nwritten<n) {
    long ntowrite = min((long)h,n-nwritten);
    long ng = in1->write(inBuf+nwritten, ntowrite);
    in0->write(inBuf+nwritten, ntowrite);
    in2->write(inBuf+nwritten, ntowrite);
    nwritten += ntowrite;
    long n_advance = 0;
    for(int k=0;k<ng;k++) {
      if(nDropped < nToDrop) {
        n_advance++;
        nDropped++;
      } else {
	if(!parent && nTrackPointsWritten%nGrainsPerFrame == 0) {
    real amod;
    if(bPreAnalyze && aPreAnalysis.n_readable()) {
	    amod = aPreAnalysis.read(aPreAnalysis.readPos);
	    aPreAnalysis.advance(1);
	  } else {
      amod = 1.0f;
	  }
	  setA(amod*a);
    setAMod(amod);	 
	  setRatio(ratio);
    setF(1.0f/ratio);
	}
	if(!parent && (nTrackPointsWritten+1)%nGrainsPerFrame == 0) {
	  setAForH(a);
	}
	if((nTrackPointsWritten+1)%nGrainsPerFrame == 0) {
	  setH(ratio);
	  nFramesWritten++;
	}
	nTrackPointsWritten++;
      }
    }
    if(n_advance) {
      in0->advance(n_advance);
      in1->advance(n_advance);
      in2->advance(n_advance);
    }
  }

#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
  
  if(sub) {
    in->write(inBuf, n);
    long ng_read = 0;
    for(int k=in->readPos;k<in->writePos;k++) {
      grain *gdown = in->read(k)->downsample();
      subIn->write(gdown, N/2/s);
      grain :: destroy(gdown);
      ng_read++;
    }
    in->advance(ng_read);  
    long n_tosub = subIn->n_readable();
    audio *subBuf = subIn->getReadBuf();
    n_tosub = subIn->read(subBuf,n_tosub);
    sub->write_(subBuf,n_tosub,a,ratio);
    subIn->advance(n_tosub);
  }
}

void subband :: stepAddFrame()
{
  if(sub) sub->stepAddFrame();
  nFramesAdded++;
}

void subband :: stepMarkFrame(int c)
{
  if(sub) sub->stepMarkFrame(c);
  nFramesMarked[c]++;
}

void subband :: stepAssignFrame(int c)
{
  if(sub) sub->stepAssignFrame(c);
  nFramesAssigned[c]++;
}

void subband :: stepSynthFrame()
{
  if(sub) sub->stepSynthFrame();
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  if(!parent) aMod.advance(1);
  aSynth.advance(1);
  fSynth.advance(1);
  nFramesSynthed++;
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
}

void subband :: stepReadFrame()
{
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  nFramesRead++;
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
}

long subband :: addInit(bool bSet)
{
  long n;
  if(sub) n = res*sub->addInit(bSet);
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  if(!sub) n = max((long)0,min((long)1,min(in1->n_readable(),4-(nFramesAdded-getFramesMarked()))));
  if(bSet) {
    nTrackPointsToAdd = n;
  }
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
  return n;
}

void subband :: addTrackPoints()
{
  if(sub) sub->addTrackPoints();
  vector<grain*> g0V;
  vector<grain*> g1V;
  vector<grain*> g2V;

#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  for(int k=in1->readPos;k<in1->readPos+nTrackPointsToAdd;k++) {
    grain *g0 = in0->read(k);
    grain *g1 = in1->read(k);
    grain *g2 = in2->read(k);
    g0V.push_back(g0);
    g1V.push_back(g1);
    g2V.push_back(g2);
  }
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif

  int size = (int)g1V.size();
  for(int k=0;k<size;k++) {
    smser->addTrackPoints(g0V[k],g1V[k],g2V[k]);
  }

#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  in0->advance(nTrackPointsToAdd);
  in1->advance(nTrackPointsToAdd);
  in2->advance(nTrackPointsToAdd);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif

}

long subband :: markInit(bool bSet, int c)
{
  long n;
  if(sub) n = res*sub->markInit(bSet,c);
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  if(!sub) n = max((long)0,min((long)1,min(nFramesAdded-nFramesMarked[c]-1,4-(nFramesMarked[c]-nFramesAssigned[c]))));
  if(bSet) {
    nTrackPointsToMark[c] = n;
  }
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
  return n;
}

void subband :: markDuplicates(int c)
{
  long ntodo = parent?1:nTrackPointsToMark[c];
  long ndone = 0;
  while(ndone<ntodo) {  
    smser->markDuplicates(nTrackPointsMarked[c],
                          parent?parent->smser:NULL,
                          sub?sub->smser:NULL,
                          c);
    if(nTrackPointsMarked[c]%res==1 || res==1) {
      if(sub) sub->markDuplicates(c);
    }
    ndone++;
    nTrackPointsMarked[c]++;
  }
}

long subband :: assignInit(bool bSet, int c)
{
  long n;
  if(sub) n = res*sub->assignInit(bSet,c);
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  if(!sub) n = max((long)0,min((long)1,min(nFramesMarked[c]-nFramesAssigned[c]-1,4+nLatency-(nFramesAssigned[c]-nFramesSynthed))));
  if(bSet) {
    nTrackPointsToAdvance[c] = n;
    nTrackPointsToAssign[c] = n;
    if(nTrackPointsToAssign[c]) {
      if(nFramesAssigned[c]==0) {
        smser->assignTrackPoints(nTrackPointsAssigned[c]++,
                                 parent?parent->smser:NULL,
                                 sub?sub->smser:NULL,
                                 c);
        smser->startNewTracks(nTrackPointsStarted[c]++,c);
      }      
    }
  }
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
  return n;
}

long subband :: assignTrackPoints(int c) 
{
  long ntodo = parent?1:nTrackPointsToAssign[c];
  long ndone = 0;
  while(ndone<ntodo) {  
    if(nTrackPointsAssigned[c]%res==0) {
      if(sub) sub->assignTrackPoints(c);
      smser->assignTrackPoints(nTrackPointsAssigned[c]++,
                               parent?parent->smser:NULL,
                               sub?sub->smser:NULL,
                               c);
      if(!parent) startNewTracks(c);
    } else {
      smser->assignTrackPoints(nTrackPointsAssigned[c]++,
                               parent?parent->smser:NULL,
                               sub?sub->smser:NULL,
                               c);
    }
    if(parent && parent->res != 1)
      parent->smser->startNewTracks(parent->nTrackPointsStarted[c]++,c);
    
    ndone++;
  }
  
  return nTrackPointsAssigned[c];
}

void subband :: startNewTracks(int c)
{
  if(!parent||!sub||nTrackPointsAssigned[c]%2==1||res==1) {
    smser->startNewTracks(nTrackPointsStarted[c]++,c);
  }
  if(sub&&(nTrackPointsAssigned[c]%2==1||res==1)) {
    sub->startNewTracks(c);
  }
}

void subband :: advanceTrackPoints(int c)
{
  long ntodo = parent?1:nTrackPointsToAdvance[c];
  long ndone = 0;

  while(ndone<ntodo) {
    if(nTrackPointsAdvanced[c]%res==0)
      if(sub) sub->advanceTrackPoints(c);
    smser->advanceTrackPoints(c);
	nTrackPointsMarked[c]--;
	nTrackPointsAssigned[c]--;
	nTrackPointsStarted[c]--;
    nTrackPointsAdvanced[c]++;
    ndone++;
  }
}

long subband :: getFramesAssigned()
{
  long assigned = LONG_MAX;
  for(int c=0;c<channels;c++) {
    if(nFramesAssigned[c] < assigned)
      assigned = nFramesAssigned[c];
  }
  return assigned;
}

long subband :: getFramesMarked()
{
  long marked = LONG_MAX;
  for(int c=0;c<channels;c++) {
    if(nFramesMarked[c] < marked)
      marked = nFramesMarked[c];
  }
  return marked;
}

long subband :: readInit(bool bSet)
{
  long n;
  if(sub) n = res*sub->readInit(bSet);
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  if(!sub) n = max((long)0,min((long)1,nFramesInFile-getFramesAssigned()));
  if(bSet) {
    nTrackPointsToAssign[0] = n;
    nTrackPointsToAssign[1] = n;
  }
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif

  return n;
}

void subband :: readTrackPointsFromFile(FILE *fp)
{
  long ntodo = parent?1:nTrackPointsToAssign[0];
  long ndone = 0;

  while(ndone<ntodo) {
    if(nTrackPointsAssigned[0]%res==0)
      if(sub) sub->readTrackPointsFromFile(fp);
    smser->readTrackPointsFromFile(fp);
#ifdef MULTITHREADED
    pthread_mutex_lock(&dataMutex);
#endif
    nTrackPointsAssigned[0]++;
    nTrackPointsAssigned[1]++;
    nTrackPointsStarted[0]++;
    nTrackPointsStarted[1]++;
    nTrackPointsMarked[0]++;
    nTrackPointsMarked[1]++;
#ifdef MULTITHREADED
    pthread_mutex_unlock(&dataMutex);
#endif
    ndone++;
  }
}


long subband :: writeTrackPointsToFile(FILE *fp) 
{
  long ntodo = parent?1:nTrackPointsToSynth;
  long ndone = 0;
  long frameBytes = 0;

  while(ndone<ntodo) {
    if(nTrackPointsSynthed%res==0)
      if(sub) frameBytes += sub->writeTrackPointsToFile(fp);
    
    frameBytes += smser->writeTrackPointsToFile(fp);
    nTrackPointsSynthed++;
    ndone++;
  }
  return frameBytes;
}

long subband :: synthInit(bool bSet) 
{
  long n;
  if(sub) n = res*sub->synthInit(bSet);
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  if(!sub) n = max((long)0,min((long)1,getFramesAssigned()-nFramesSynthed-nLatency));
  if(bSet) {
    nTrackPointsSynthed = 0;
    nTrackPointsToSynth = n;
  }
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
  return n;
}

void subband :: synthTracks() 
{
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  real a = aSynth.read(aSynth.readPos);
  real f0 = fSynth.read(fSynth.readPos);
  real f1;
  if(fSynth.n_readable()>=2)
    f1 = fSynth.read(fSynth.readPos+1);
  else
    f1 = f0;
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
  long ntodo = parent?1:nTrackPointsToSynth;
  long ndone = 0;

  while(ndone<ntodo) {
    if(nTrackPointsSynthed%res==0)
      if(sub) sub->synthTracks();   
    real df = (f1-f0)/(real)nTrackPointsToSynth;
    smser->synthTracks(a,f0+nTrackPointsSynthed*df,f0+(nTrackPointsSynthed+1)*df);
    nTrackPointsSynthed++;
    ndone++;
  }
}

void subband :: readSubSamples()
{
  if(sub) sub->readSubSamples();
  if(sub) {
    audio fromSub[SUB_BUF_SIZE];
    long n_fromsub = 0;
    do {
      n_fromsub = min((long)SUB_BUF_SIZE,sub->n_readable());
      n_fromsub = sub->read(fromSub,n_fromsub);
      subOut->write(fromSub, n_fromsub);
    } while(n_fromsub>0);
  }
}

long subband :: write(audio *inBuf, long n, real a, real ratio)
{
  write_(inBuf, n, a, ratio);
  return n;
}

void subband :: process()
{
  do { 
    addInit(true);
    addTrackPoints();
    if(nTrackPointsToAdd) stepAddFrame();
  } while(nTrackPointsToAdd);


  for(int c=0;c<channels;c++) {
    do { 
      markInit(true,c);
      markDuplicates(c);
      if(nTrackPointsToMark[c]) stepMarkFrame(c);
    } while(nTrackPointsToMark[c]);
    
    do {
      assignInit(true,c);
      assignTrackPoints(c);
      advanceTrackPoints(c);
      if(nTrackPointsToAssign[c]) stepAssignFrame(c);
    } while(nTrackPointsToAssign[c]);
  }
}

void subband :: setFramesInFile(long frames) 
{
  if(sub) sub->setFramesInFile(frames);
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  nFramesInFile = frames;
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
}

long subband :: readFromFile(FILE *fp, real a, real ratio)
{
  long samples = 0; 
  if(readInit(true)) {
    int framesize;
    real amod;
    fread(&framesize,sizeof(int),1,fp);
    fread(&amod,sizeof(real),1,fp);
    readTrackPointsFromFile(fp);
    for(int c=0;c<channels;c++)
      stepAssignFrame(c);
    samples += framesize;
#ifdef MULTITHREADED
    pthread_mutex_lock(&dataMutex);
#endif    
    setA(1.0f+amod*(a-1.0f));
    setAMod(amod);
    setF(1.0f/ratio);
    setRatio(ratio);
    setFrameSize(framesize,a,ratio);
#ifdef MULTITHREADED
    pthread_mutex_unlock(&dataMutex);
#endif    
  }
  return samples;
}

void subband :: writeFramePositionsToFile(FILE *fp)
{
  long frames = 0;
  // samples, frames, channels, quality
  long offset = 2*sizeof(long) + sizeof(int) + sizeof(sbsms_quality);
  unsigned short maxtrackindex = ta->size();
  fseek(fp,offset,SEEK_SET);
  fwrite(&maxtrackindex,sizeof(unsigned short),1,fp); offset += sizeof(unsigned short);
  fseek(fp,0,SEEK_END);
  for(int k=frameBytes.readPos+nFramesSkipped; k<frameBytes.writePos;k++) {
    long samples = inputFrameSize.read(k);
    int bytes = frameBytes.read(k);
    fwrite(&offset,sizeof(long),1,fp);
    fwrite(&samples,sizeof(long),1,fp);
    offset += bytes;
    frames++;
  }
  //samples
  offset = sizeof(long);
  fseek(fp,offset,SEEK_SET);
  fwrite(&frames,sizeof(long),1,fp);
}

long subband :: getFramesWrittenToFile()
{
  long samples = 0;
  bool bRead;
  do {
#ifdef MULTITHREADED
    pthread_mutex_lock(&dataMutex);
#endif    
    bRead = (nFramesSynthed > nFramesRead);
#ifdef MULTITHREADED
    pthread_mutex_unlock(&dataMutex);
#endif    
    if(bRead) {
      stepReadFrame();
      if(nFramesRead > nFramesSkipped)
	samples += inputFrameSize.read(nFramesSynthed);
    }
  } while(bRead);
  return samples;
}

long subband :: writeTracksToFile(FILE *fp)
{
  long samples = 0;
  bool bReady = false;
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif    
  if(inputFrameSize.n_readable() > nFramesSynthed) bReady = true;
  if(!bReady) {
#ifdef MULTITHREADED
    pthread_mutex_unlock(&dataMutex);
#endif    
    return 0;
  }
  int framesize = inputFrameSize.read(nFramesSynthed);
  real amod = aMod.read(aMod.readPos);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif    
  samples += framesize;
  int bytes = 0;
  if(fp) {
    fwrite(&framesize,sizeof(int),1,fp); bytes += sizeof(int);
    fwrite(&amod,sizeof(real),1,fp); bytes += sizeof(real);
  } else {
    nFramesSkipped++;
  }
  bytes += writeTrackPointsToFile(fp);
  frameBytes.write(bytes);
  frameRatio.advance(1);
  outputFrameSize.advance(1);
  return samples;
}

long subband :: writeToFile(FILE *fp)
{
  long samples = 0; 
  if(synthInit(true)) {
    samples = writeTracksToFile(fp);
    stepSynthFrame();
  }
  return samples;
}

long subband :: synth()
{
  long frames = 0;
  do {
    if(synthInit(true)) {
      synthTracks();
      stepSynthFrame();
      frames++;
    }
  } while(nTrackPointsToSynth);
  return frames;
}

long subband :: zeroPad()
{
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  frameRatio.write(lastFrameRatio);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
  long samples = zeroPad_();
  return samples;
}

long subband :: zeroPad_()
{
  if(sub) sub->zeroPad_();
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  setFrameSize(lastInputFrameSize,lastFrameA,lastFrameRatio);
  smser->zeroPad(lastOutputFrameSize);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
  return lastOutputFrameSize;
}

bool subband :: isframe_readable()
{
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  if(!outputFrameSize.n_readable())
    return false;
  long n = outputFrameSize.read(outputFrameSize.readPos);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
  return (n_readable()>=n);
}

long subband :: getSamplesQueued()
{
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  long n = (long)round2int(samplesQueued);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
  return n;
}

long subband :: getFramesQueued()
{
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  long n = outputFrameSize.n_readable();
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
  return n;
}

long subband :: getFramesAtFront()
{
  if(sub) return sub->getFramesAtFront();
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  long n = in1->n_readable();
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
  return n;
}

long subband :: getFramesAtBack()
{
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  long n =  getFramesAssigned() - nFramesSynthed;
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
  return n;
}

long subband :: getLastInputFrameSize()
{
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  long n = lastInputFrameSize;
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
  return n;
}

long subband :: n_readable()
{
#ifdef MULTITHREADED
  pthread_mutex_lock(&bufferMutex);
#endif
  long n = outMixer->n_readable();
#ifdef MULTITHREADED
  pthread_mutex_unlock(&bufferMutex);
#endif
  return n;
}

long subband :: read(audio *buf, real *ratio0, real *ratio1) 
{
  readSubSamples();
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  long n = 0;
  if(outputFrameSize.n_readable() >= 2) {
    n = outputFrameSize.read(outputFrameSize.readPos);
    if(n_readable()>=n) {
      real rat0 = frameRatio.read(frameRatio.readPos);
      real rat1 = frameRatio.read(frameRatio.readPos+1);
      if(ratio0 != NULL) *ratio0 = rat0;
      if(ratio1 != NULL) *ratio1 = rat1;
      samplesQueued -= n*0.5f*(rat0+rat1);
      outputFrameSize.advance(1);
      inputFrameSize.advance(1);
      frameRatio.advance(1);
#ifdef MULTITHREADED
      pthread_mutex_unlock(&dataMutex);
#endif
      if(buf) {
        stepReadFrame();
      }
      n = read(buf, n);
    } else {
      n = 0;
#ifdef MULTITHREADED
      pthread_mutex_unlock(&dataMutex);
#endif
    }
  } else {
#ifdef MULTITHREADED
    pthread_mutex_unlock(&dataMutex);
#endif
  }

  return n;
}

long subband :: read(audio *buf, long n)
{
  if(n==0)
    return 0;
  long n_read = n;
#ifdef MULTITHREADED
  pthread_mutex_lock(&bufferMutex);
#endif
  if(buf) n_read = outMixer->read(buf, n_read);
  outMixer->advance(n_read);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&bufferMutex);
#endif

  return n_read;
}

void subband :: writingComplete()
{
  if(sub) sub->writingComplete();
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  nLatency = 0;
  bWritingComplete = true;
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
}

long subband :: preAnalyze(audio *buf, long n, real a, real ratio)
{
  long nwritten = 0;
  while(nwritten<n) {
    long ntowrite = min((long)(inPre->h),n-nwritten);
    inPre->write(buf+nwritten,ntowrite);

    nwritten += ntowrite;
    
    long ng_read = 0;
    for(int k=inPre->readPos;k<inPre->writePos;k++) {
      if(nDropped < nToDrop) {
        nDropped++;
      } else {
        if(!parent && (nTrackPointsWritten)%nGrainsPerFrame == 0) {
          grain *g = inPre->read(k);
          grain::referenced(g);
          real o = calculateOnset(gPrev,g);
          onset.write(o);
          if(gPrev) grain::forget(gPrev);
          gPrev = g;
          setA(a);
        }
        if(!parent && (nTrackPointsWritten+1)%nGrainsPerFrame == 0) {
          setAForH(a);
        }
        if((nTrackPointsWritten+1)%nGrainsPerFrame == 0) {
          setH(ratio);
        }
        nTrackPointsWritten++;
      }
      ng_read++;
    }
    inPre->advance(ng_read);
  }
  long samples = 0;
  while(inputFrameSize.n_readable()) {
    samples += inputFrameSize.read(inputFrameSize.readPos);
    inputFrameSize.advance(1);
    outputFrameSize.advance(1);
  }
  return samples;
}

real subband :: getOnset(long k)
{
  real tot = 2.0f*onset.read(k);
  real c = 2.0f;
  if(k-1>=onset.readPos) {
    tot += onset.read(k-1);
    c += 1.0f;
  }
  if(k+1<onset.writePos) {
    tot += onset.read(k+1);
    c += 1.0f;
  }
  return tot / c;
}

void subband :: preAnalyzeComplete()
{
  long k0 = onset.readPos;
  long k1 = onset.writePos;
  long kstart = 0;
  long kend = 0;

  for(long k=k0;k<k1;k++) {
    bool bOnset = false;
    if(k==k0) {
      bOnset = true;
    } else {
      real o0 = getOnset(k);
      if(o0 < 0.1f) continue;
      real o1 = getOnset(k-1);
      if(o0 < 1.1f*o1) continue;
      if(o0 < 0.22f) continue;
      bOnset = ((o0 > 0.4f) || (o0>1.4f*o1));
      if(!bOnset && k>k0+1) {
        real o2 = getOnset(k-2);
        bOnset = ((o0 > 1.2f*o1) && (o1 > 1.2f * o2));
        if(!bOnset && k>k0+2) {
          real o3 = getOnset(k-3);
          bOnset = ((o0 > 0.3f) && (o1 > 1.1f * o2) && (o2 > 1.1f * o3));
        }
      }
    }
    if(bOnset) {
      if(kend != 0) {
        calculateA(kstart,kend);
      }
      kstart = kend;
      kend = k;
    }
  }
  calculateA(kstart,k1);
  aPreAnalysis.write(1.0f);
  aPreAnalysis.advance(1);
}

void subband :: calculateA(long kstart, long kend)
{
  real oMax = 0.0f;
  real dTotal = 0.0f;

  for(long k=kstart;k<kend;k++) {
    real o = getOnset(k);
    if(o > oMax) oMax = o;
  }

  oMax *= 1.3;

  for(long k=kstart;k<kend;k++) {
    real o = getOnset(k);
    real d = oMax - o;
    dTotal += d;
  }

  real aAllot = (real)(kend-kstart);
  for(long k=kstart;k<kend;k++) {
    real o = getOnset(k);
    real d = oMax - o;
    real da;
    if(dTotal == 0.0 || k+1==kend) {
      da = aAllot;
    } else {
      da = d/dTotal*aAllot;
    }

    dTotal -= d;
    aAllot -= da;
    aPreAnalysis.write(da);
  }

}

real subband :: calculateOnset(grain *g1, grain *g2)
{  
  _c2evenodd(g2->x, x2[0]->x, x2[1]->x, g2->N);
  real o = 1.0f;
  if(g1!=NULL) {
    int Nover2 = N/2;
    int nOnset = 0;
    int nThresh = 0;
    for(int k=0;k<Nover2;k++) {
      real m2 = norm2(x2[0]->x[k]) + norm2(x2[1]->x[k]);
      real m1 = norm2(x1[0]->x[k]) + norm2(x1[1]->x[k]);
      bool bThresh = (m2 > 1e-6);
      bool bOnset = (m2 > 2.0f*m1) && bThresh;
      if(bOnset) nOnset++;
      if(bThresh) nThresh++;
    }
    if(nThresh == 0)
      o = 0.0f;
    else
      o = (real)nOnset/(real)nThresh;
  } 
  memcpy(x1[0]->x,x2[0]->x,g2->N*sizeof(audio));
  memcpy(x1[1]->x,x2[1]->x,g2->N*sizeof(audio));
  return o;
}

}
