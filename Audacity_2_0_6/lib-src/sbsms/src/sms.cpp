#include "sms.h"
#include "real.h"
#include "utils.h"
#include "dBTable.h"
#include <stdlib.h>
#include <math.h>
#include <set>
using namespace std;

namespace _sbsms_ {

SMS :: SMS(SMS *lo, int N, int band, int bandMax, int h, int res, int N0, int N1, int N2, int channels, audio *peak2)
{
  this->lo = lo;
  if(lo) lo->hi = this;
  hi = NULL;
  this->band = band;
  this->h = h;
  this->h1 = (double)(h<<band);
  this->res = res;
  this->resMask = res - 1;
  this->channels = channels;
  this->N = N;
  this->Nover2 = N/2;
  float pad2 = (float)N/(float)N2;
  float pad1 = (float)N/(float)N1;
  float pad0 = (float)N/(float)N0;
  M = (float)(1<<band);
  peakThresh = 1e-8f;

  float maxDF2 = square(0.005f * (float)h) / M;
  maxDF = sqrt(maxDF2);
  maxCost2 = 0.5f * maxDF2;
  dMCoeff2 = 0.002f * maxDF2;
  
  float maxDF2SplitMerge = square(0.001f * (float)h) / M;
  maxDFSplitMerge = sqrt(maxDF2SplitMerge);
  maxCost2SplitMerge = 1.0f * maxDF2SplitMerge;
  dMCoeff2SplitMerge = 0.006f * maxDF2SplitMerge;

  maxDFMatch = .06f / M;
  float maxDF2Match = square(maxDFMatch);
  dMCoeff2Match = 0.0075f * maxDF2Match;
  maxCost2Match = 0.8f * maxDF2Match;

  maxDFStereo = .04f / (float)M;
  float maxDF2Stereo = square(maxDFStereo);
  dMCoeff2Stereo = 0.005f * maxDF2Stereo;
  maxCost2Stereo = 1.0f*maxDF2Stereo;

  peakWidth0 = lrintf(pad0*(float)N*0.0055f) + 1;
  peakWidth1 = lrintf(pad1*(float)N*0.0055f) + 1;
  peakWidth2 = lrintf(pad2*(float)N*0.0055f) + 1;
  minTrackSize = max(384/(h<<band),N2/h/2);
  minCutSep2 = max((int)lrintf(0.008f * (float)N),peakWidth1);
  minCutSep1 = max((int)lrintf(0.011f * (float)N),peakWidth0);
  if(band==bandMax) kLo = 1;
  else kLo = max(1L,lrintf(floor(0.5f*(float)N/(float)lo->N*(float)lo->kHi-maxDFMatch*M/TWOPI*(float)N)));
  if(band==0) kHi = Nover2;
  else kHi = max(1L,lrintf(0.4785f * N)-peakWidth0*2);
  kStart = max(1,kLo-peakWidth0);
  kEnd = min(Nover2-1,kHi+peakWidth0*2);
  float kNorm = TWOPI / (float)(M * N);
  maxFHi = (float)kHi * kNorm + maxDF;
  minFLo = (float)kLo * kNorm - maxDF;
  if(lo) maxFMatchM = (float)lo->kHi * TWOPI / (float)(lo->N * M * 2) + maxDFMatch;
  else maxFMatchM = 0.0f;
  minFMatchL = (float)kLo * kNorm - maxDFMatch;
  if(lo) maxFMid = (float)lo->kHi * TWOPI / (float)(lo->N * M * 2) + maxDF;
  else maxFMid = 0.0f;
  if(lo) lo->minFMid = (float)kLo * kNorm - lo->maxDF;
  if(lo && lo->lo) {
    minK = max(1L,(lrintf(0.25f * (float)N / (float)lo->lo->N * (float)lo->lo->kHi + peakWidth0)));
  } else {
    minK = 1;
  }
  maxK = min(kEnd,kHi + peakWidth0);
  localFavorRatio = 1.1f;
  mNorm = MScale * MScale * 16.061113032124002f * pad2 / square((float)N);
  for(int c=0; c<channels; c++) {
    bAssignDone[c] = false;
    addtime[c] = 0;
    assigntime[c] = 0;
    trial2time[c] = 0;
    trial1time[c] = 0;
    synthtime[c] = 0;
    for(int k=1; k<256; k++) {
      trackIndex[c].push(k);
    }
    trial2Buf[c] = (float*)malloc(h*res*sizeof(float));
    trial2RingBuf[c] = new ArrayRingBuffer<float>(0);
    dmag1[c] = (float*)malloc(N*sizeof(float));
    mag11[c] = (float*)malloc((Nover2+1)*sizeof(float));
    x10[c] = (audio*)malloc(N*sizeof(audio));
    x11[c] = (audio*)malloc(N*sizeof(audio));
    trial1Buf[c] = (float*)malloc(h*res*sizeof(float));
    trial1RingBuf[c] = new ArrayRingBuffer<float>(0);
    dmag0[c] = (float*)malloc(N*sizeof(float));
    mag01[c] = (float*)malloc((Nover2+1)*sizeof(float));
    x00[c] = (audio*)malloc(N*sizeof(audio));
    x01[c] = (audio*)malloc(N*sizeof(audio));
    mag2[c] = (float*)malloc((Nover2+1)*sizeof(float));
    dec2[c] = (float*)malloc(N*sizeof(float));
    x2[c] = (audio*)malloc(N*sizeof(audio));
#ifdef MULTITHREADED
    pthread_mutex_init(&renderMutex[c],NULL);
    pthread_mutex_init(&trackMutex[c],NULL);
    pthread_mutex_init(&sliceMutex[c],NULL);
    pthread_mutex_init(&trial2Mutex[c],NULL);
    pthread_mutex_init(&trial1Mutex[c],NULL);
    pthread_mutex_init(&magMutex[c],NULL);
#endif
  }
  h2cum = 0.0;
  adjust2time = 0;
  adjust1time = 0;
  trial2GrainBuf = new GrainBuf(N,h,N1,hannpoisson);
  trial1GrainBuf = new GrainBuf(N,h,N0,hannpoisson);
  peak20 = (float*)calloc(2*N,sizeof(float));
  peak2N = peak20 + N;
  for(int k=-Nover2;k<=Nover2;k++) {
    peak2N[k] = norm2(peak2[(k+N)%N]);
  }
}

SMS :: ~SMS()
{
  for(int c=0;c<channels;c++) {
    while(!mag1Queue[c].empty()) {
      delete mag1Queue[c].front();
      mag1Queue[c].pop();
    }
    while(!mag0Queue[c].empty()) {
      delete mag0Queue[c].front();
      mag0Queue[c].pop();
    }
    set<Track*> tracks;
    for(list<Track*>::iterator i=assignTracks[c].begin(); 
        i != assignTracks[c].end(); 
        ++i ) {
      tracks.insert(*i);
    }
    for(list<Track*>::iterator i=renderTracks[c].begin(); 
        i != renderTracks[c].end(); 
        ++i ) {
      tracks.insert(*i);
    }
    for(set<Track*>::iterator i=tracks.begin(); 
        i != tracks.end(); 
        ++i ) {
      delete *i;
    }
    set<Slice*> slices;
    while(!adjust2SliceQueue[c].empty()) {
      slices.insert(adjust2SliceQueue[c].front());
      adjust2SliceQueue[c].pop();
    }
    while(!adjust1SliceQueue[c].empty()) {
      slices.insert(adjust1SliceQueue[c].front());
      adjust1SliceQueue[c].pop();
    }
    for(long k=sliceBuffer[c].readPos; k<sliceBuffer[c].writePos; k++) {
      slices.insert(sliceBuffer[c].read(k));
    }
    for(set<Slice*>::iterator i = slices.begin();
        i != slices.end();
        ++i) {
      Slice *s = *i;
      TrackPoint *tp = s->bottom;
      delete s;
      while(tp) {
        TrackPoint *tpn = tp->pn;
        if(!tp->owner) tp->destroy();
        tp = tpn;
      }
    }
    free(trial2Buf[c]);
    delete trial2RingBuf[c];
    free(trial1Buf[c]);
    delete trial1RingBuf[c];
    free(dmag1[c]);
    free(mag11[c]);
    free(x10[c]);
    free(x11[c]);
    free(dmag0[c]);
    free(mag01[c]);
    free(x00[c]);
    free(x01[c]);
    free(mag2[c]);
    free(x2[c]);
    free(dec2[c]);
  }
  free(peak20);
  delete trial2GrainBuf;
  delete trial1GrainBuf;
}

void SMS :: trial2Start(int c)
{
  if(band >= minTrial2Band) {
    memset(trial2Buf[c],0,h*res*sizeof(float));
  }
}

void SMS :: trial2End(int c)
{
  if(band < minTrial2Band) return;
#ifdef MULTITHREADED
  pthread_mutex_lock(&trial2Mutex[c]);
#endif
  trial2RingBuf[c]->write(trial2Buf[c],h*res);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&trial2Mutex[c]);
#endif
}

void SMS :: trial2(int c)
{
#ifdef MULTITHREADED
  pthread_mutex_lock(&trackMutex[c]);
#endif
  for(list<Track*>::iterator tt = renderTracks[c].begin(); 
      tt != renderTracks[c].end();
      ++tt) {
    Track *t = (*tt);
    if(trial2time[c] >= t->start) {
      if(trial2time[c] > t->last) continue;
      t->updateM(trial2time[c],synthModeTrial2);
      if(hi && hi->band >= minTrial2Band) {
        float f = 0.5f*M;
        t->updateFPH(trial2time[c],synthModeTrial2,h<<1,f,f);
        t->synth(hi->trial2Buf[c],trial2time[c],h<<1,synthModeTrial2,c);
      }
      if(lo && lo->band >= minTrial2Band) {
        float f = 2.0f*M;
        t->updateFPH(trial2time[c],synthModeTrial2,h>>1,f,f);
        t->synth(lo->trial2Buf[c]+(trial2time[c]&(res*lo->res-1))*(h>>1),trial2time[c],h>>1,synthModeTrial2,c);
      }
      if(band >= minTrial2Band) {
        float f = M;
        t->updateFPH(trial2time[c],synthModeTrial2,h,f,f);
      }
    } else {
      break;
    }
  }
#ifdef MULTITHREADED
  pthread_mutex_unlock(&trackMutex[c]);
#endif
  trial2time[c]++;
}

void SMS :: trial1Start(int c)
{
  if(band >= minTrial1Band) {
    memset(trial1Buf[c],0,h*res*sizeof(float));
  }
}

void SMS :: trial1End(int c)
{
  if(band < minTrial1Band) return;
#ifdef MULTITHREADED
  pthread_mutex_lock(&trial1Mutex[c]);
#endif
  trial1RingBuf[c]->write(trial1Buf[c],h*res);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&trial1Mutex[c]);
#endif
}

void SMS :: trial1(int c)
{
#ifdef MULTITHREADED
  pthread_mutex_lock(&trackMutex[c]);
#endif
  for(list<Track*>::iterator tt = renderTracks[c].begin(); 
      tt != renderTracks[c].end();
      ++tt) {
    Track *t = (*tt);
    if(trial1time[c] >= t->start) {
      if(trial1time[c] > t->last) continue;
      t->updateM(trial1time[c],synthModeTrial1);
      if(hi && hi->band >= minTrial1Band) {
        float f = 0.5f*M;
        t->updateFPH(trial1time[c],synthModeTrial1,h<<1,f,f);
        t->synth(hi->trial1Buf[c],trial1time[c],h<<1,synthModeTrial1,c);
      }
      if(lo && lo->band >= minTrial1Band) {
        float f = 2.0f*M;
        t->updateFPH(trial1time[c],synthModeTrial1,h>>1,f,f);
        t->synth(lo->trial1Buf[c]+(trial1time[c]&(res*lo->res-1))*(h>>1),trial1time[c],h>>1,synthModeTrial1,c);
      }
      if(band >= minTrial1Band) {
        float f = M;
        t->updateFPH(trial1time[c],synthModeTrial1,h,f,f);
        t->synth(trial1Buf[c]+(trial1time[c]&resMask)*h,trial1time[c],h,synthModeTrial1,c);
      }
    } else {
      break;
    }
  }
#ifdef MULTITHREADED
  pthread_mutex_unlock(&trackMutex[c]);
#endif
  trial1time[c]++;
}

void SMS :: adjust2()
{
  Slice* slice[2];
  for(int c=0; c<channels; c++) {
#ifdef MULTITHREADED
    pthread_mutex_lock(&sliceMutex[c]);
#endif
    slice[c] = adjust2SliceQueue[c].front(); adjust2SliceQueue[c].pop();
#ifdef MULTITHREADED
    pthread_mutex_unlock(&sliceMutex[c]);
#endif
  }
  if(band >= minTrial2Band) {
#ifdef MULTITHREADED
    for(int c=0; c<channels; c++) {
      pthread_mutex_lock(&trial2Mutex[c]);
    }
#endif
    adjustInit(trial2RingBuf,trial2GrainBuf);
#ifdef MULTITHREADED
    for(int c=channels-1; c>=0; c--) {
      pthread_mutex_unlock(&trial2Mutex[c]);
    }
#endif
    adjust(trial2GrainBuf,mag1Queue,minCutSep1,mag11,dmag1,x11,adjust2time,slice);
  }
  if(channels == 2) {
    for(int c=0; c<2; c++) {
      for(TrackPoint *pc = slice[c]->bottom;
          pc;
          pc = pc->pn) {
        pc->bOwned = false;
        pc->cont = NULL;
      }
    }
    for(int c=0; c<2; c++) {
      int c2 = (c==0?1:0);
      TrackPoint *begin = slice[c2]->bottom;
      for(TrackPoint *pc = slice[c]->bottom;
          pc;
          pc = pc->pn) {
        float F;
        pc->cont = nearestForward(&begin,pc,&F,maxCost2Stereo,maxDFStereo,dMCoeff2Stereo);
      }
    }
    for(TrackPoint *p0 = slice[0]->bottom;
        p0;
        p0 = p0->pn) {
      TrackPoint *p1 = p0->cont;
      if(p1 && p1->cont == p0) {
        p0->dupStereo = p1;
        p1->dupStereo = p0;
      }
    }
  }
  adjust2time++;
}

void SMS :: adjust1(float stretch, float pitch0, float pitch1)
{
  Slice* slice[2];
  for(int c=0; c<channels; c++) {
#ifdef MULTITHREADED
    pthread_mutex_lock(&sliceMutex[c]);
#endif
    slice[c] = adjust1SliceQueue[c].front(); adjust1SliceQueue[c].pop();
#ifdef MULTITHREADED
    pthread_mutex_unlock(&sliceMutex[c]);
#endif
  }
  if(band >= minTrial1Band) {
#ifdef MULTITHREADED
    for(int c=0; c<channels; c++) {
      pthread_mutex_lock(&trial1Mutex[c]);
    }
#endif
    adjustInit(trial1RingBuf,trial1GrainBuf);
#ifdef MULTITHREADED
    for(int c=channels-1; c>=0; c--) {
      pthread_mutex_unlock(&trial1Mutex[c]);
    }
#endif
    adjust(trial1GrainBuf,mag0Queue,minCutSep1,mag01,dmag0,x01,adjust1time,slice);
  }
  for(int c=0; c<channels; c++) {
    delete slice[c];
  }
  double h2 = stretch * h1;
  h2cum += h2;
  int h2i = lrint(h2cum);
  h2cum -= h2i; 
  for(int c=0; c<channels; c++) {
#ifdef MULTITHREADED
    pthread_mutex_lock(&renderMutex[c]);
#endif
    nRender[c].push(h2i);
#ifdef MULTITHREADED
    pthread_mutex_unlock(&renderMutex[c]);
#endif
  }
  list<TrackPoint*> dupStereoPostponed;
  for(int c=0; c<channels; c++) {
#ifdef MULTITHREADED
    pthread_mutex_lock(&trackMutex[c]);
#endif
    for(list<Track*>::iterator tt = renderTracks[c].begin(); 
        tt != renderTracks[c].end();
        ++tt) {
      Track *t = (*tt);
      if(adjust1time >= t->start) {
        if(adjust1time <= t->last) {
          TrackPoint *tp = t->updateFPH(adjust1time,synthModeOutput,h2i,pitch0,pitch1);
          if(tp) dupStereoPostponed.push_back(tp);
        }
      } else {
        break;
      }
    }
#ifdef MULTITHREADED
    pthread_mutex_unlock(&trackMutex[c]);
#endif  
  }
  for(list<TrackPoint*>::iterator tpi = dupStereoPostponed.begin();
      tpi != dupStereoPostponed.end();
      tpi++) {
    TrackPoint *tp = (*tpi);
    tp->phSynth = canon2PI(tp->dupStereo->phSynth + tp->ph - tp->dupStereo->ph);
  }
  adjust1time++;
}

int SMS :: findCut(float *dmag, int k0, int maxK) 
{
  int k;
  for(k = max(1,k0); k <= maxK; k++) {
    float dd0 = dmag[k+1] - dmag[k];
    if(dd0 > 0.0f) {
      float d02 = square(dmag[k+1] + dmag[k]);
      if(dd0 * square(dmag[k] + dmag[k-1]) > (dmag[k] - dmag[k-1]) * d02
         &&
         dd0 * square(dmag[k+2] + dmag[k+1]) > (dmag[k+2] - dmag[k+1]) * d02) {
        break;
      }
    }
  }
  return k;
}

void SMS :: adjustInit(ArrayRingBuffer<float> **trialRingBuf,
                       GrainBuf *trialGrainBuf)
{
  long n = trialRingBuf[0]->nReadable();
  for(int c=1; c<channels; c++) {
    n = min(n,trialRingBuf[c]->nReadable());
  }
  long ndone = 0;
  while(n) {
    audio abuf[512];
    long ntodo = min(512L,n);
    for(int c=0; c<channels; c++) {
      float *fbuf = trialRingBuf[c]->getReadBuf();
      for(int k=0; k<ntodo; k++) {
        abuf[k][c] = fbuf[ndone+k];
      }
    }
    for(int c=channels; c<2; c++) {
      for(int k=0; k<ntodo; k++) {
        abuf[k][c] = 0.0f;
      }
    }
    trialGrainBuf->write(abuf,ntodo);
    ndone += ntodo;
    n -= ntodo;
  }
  for(int c=0; c<channels; c++) {
    trialRingBuf[c]->advance(ndone);
  }
}

void SMS :: adjust(GrainBuf *trialGrainBuf,
                   queue<float*> *magQueue,
                   int minCutSep,
                   float **_mag1,
                   float **_dmag1,
                   audio **x1,
                   const TimeType &time,
                   Slice **slices)
{
  grain *g = trialGrainBuf->read(trialGrainBuf->readPos);
  g->analyze();
  for(int c=0; c<channels; c++) {    
    Slice *slice = slices[c];
    TrackPoint *p = slice->bottom;
    if(c == 0) {
      c2even(g->x, x1[0], N);
    } else {
      c2odd(g->x, x1[1], N);
    }
    float *mag1 = _mag1[c];
    calcmags(mag1, x1[c]);
#ifdef MULTITHREADED
    pthread_mutex_lock(&magMutex[c]);
#endif
    float *mag0 = magQueue[c].front(); magQueue[c].pop();
#ifdef MULTITHREADED
    pthread_mutex_unlock(&magMutex[c]);
#endif
    if(p) {
      float *dmag = _dmag1[c];
      list<int> cuts;
      int k3 = min(Nover2,maxK+2);
      dmag[0] = mag1[0];
      for(int k=max(1,minK-2); k<k3; k++) {
        dmag[k] = mag1[k] - mag1[k-1];
      }
      int k = minK;
      while(true) {
        k = findCut(dmag,k+1,maxK);
        if(k >= maxK) {
          break;
        } else  {
          cuts.push_back(k);
        }
      }
      bool bDone = false;
      while(!bDone) {
        bDone = true;
        for(list<int>::iterator i = cuts.begin();
            i != cuts.end();
            ++i) {
          int k0 = *i;
          list<int>::iterator ibad = cuts.end();
          list<int>::iterator i2 = i;
          ++i2;
          float maxY = 0.0f;
          for(;
              i2 != cuts.end();
              ++i2) {
            int k2 = *i2;
            if(k2 - k0 >= minCutSep) break;
            float y = mag0[k2] * mag1[k2];
            if(y >= maxY) {
              maxY = y;
              ibad = i2;
            }
            k0 = k2;
          }
          if(ibad != cuts.end()) {
            if(mag0[*i] * mag1[*i] > maxY) {
              ibad = i;
            }
            cuts.erase(ibad);
            bDone = false;
            break;
          }
        }
      }
      cuts.push_front(minK);
      cuts.push_back(maxK);
      list<int>::iterator i = cuts.begin();
      while(p) {
        int k0 = *i;
        ++i;
        if(i == cuts.end()) break;
        int k2 = *i;
        if(p->x > k2) continue;
        float m0 = 0.0f;
        float m1 = 0.0f;
        for(int k=k0;k<=k2;k++) {
          m0 += mag0[k];
          m1 += mag1[k];
        }
        float s = (m1>m0?sqrt(m0/m1):1.0f);
        while(p && p->x <= k2) {
          p->m *= s;
          p = p->pn;
        }
      }
    }
    free(mag0);
  }
  trialGrainBuf->advance(1);
}

void SMS :: render(int c, list<SBSMSRenderer*> &renderers)
{
#ifdef MULTITHREADED
  pthread_mutex_lock(&renderMutex[c]);
#endif
  int n = nRender[c].front(); nRender[c].pop();
#ifdef MULTITHREADED
  pthread_mutex_unlock(&renderMutex[c]);
#endif
  TimeType time = synthtime[c];
  for(list<SBSMSRenderer*>::iterator i = renderers.begin(); i != renderers.end(); ++i) {
    SBSMSRenderer *renderer = *i;
    renderer->startTime(c,time,n);
  }
#ifdef MULTITHREADED
  pthread_mutex_lock(&trackMutex[c]);
#endif
  for(list<Track*>::iterator tt = renderTracks[c].begin(); 
      tt != renderTracks[c].end();) {
    Track *t = (*tt);
    if(t->bEnded && time > t->last) {
      list<Track*>::iterator eraseMe = tt;
      ++tt;
      renderTracks[c].erase(eraseMe);
      delete t;
    } else if(time >= t->start) {
      if(time <= t->last) {
        t->updateM(time,synthModeOutput);
        for(list<SBSMSRenderer*>::iterator i = renderers.begin(); i != renderers.end(); ++i) {
          SBSMSRenderer *renderer = *i;
          renderer->render(c,t);
        }
        t->step(time);
      }
      ++tt;
    } else {
      break;
    }
  }
#ifdef MULTITHREADED
  pthread_mutex_unlock(&trackMutex[c]);
#endif  
  for(list<SBSMSRenderer*>::iterator i = renderers.begin(); i != renderers.end(); ++i) {
    SBSMSRenderer *renderer = *i;  
    renderer->endTime(c);
  }
  synthtime[c]++;
}

TrackPoint *SMS :: nearestForward(TrackPoint **begin, TrackPoint *tp0, float *minCost2, float maxCost2, float maxDF, float dMCoeff2, float dNCoeff2) 
{
  *minCost2 = TrackPointNoCont;
  float minF = tp0->f - maxDF;
  float maxF = tp0->f + maxDF;
  float maxDF2 = square(maxDF);
  while((*begin) && (*begin)->f < minF) {
    (*begin) = (*begin)->pn;
  }
  TrackPoint *mintp1 = NULL;
  for(TrackPoint *tp1 = (*begin);
      tp1;
      tp1 = tp1->pn) {
    if(tp1->bOwned) continue;
    float df2 = square(tp1->f - tp0->f);
    if(df2 > maxDF2) break;
    float dM2 = dBApprox(tp1->m2,tp0->m2);
    float cost2 = (df2+dMCoeff2*dM2);
    if(cost2 > maxCost2) continue;
    if(cost2 < (*minCost2)) {
      (*minCost2) = cost2;
      mintp1 = tp1;
    }
  }
  return mintp1;
}

TrackPoint *SMS :: nearestReverse(TrackPoint **begin, TrackPoint *tp0, float *minCost2, float maxCost2, float maxDF, float dMCoeff2, float dNCoeff2) 
{
  *minCost2 = TrackPointNoCont;
  float minF = tp0->f - maxDF;
  float maxF = tp0->f + maxDF;
  float maxDF2 = square(maxDF);
  while((*begin) && (*begin)->f > maxF) {
    (*begin) = (*begin)->pp;
  }
  TrackPoint *mintp1 = NULL;
  for(TrackPoint *tp1 = (*begin);
      tp1;
      tp1 = tp1->pp) {
    if(tp1->bOwned) continue;
    float df2 = square(tp1->f - tp0->f);
    if(df2 > maxDF2) break;
    float dM2 = dBApprox(tp1->m2,tp0->m2);
    float cost2 = (df2+dMCoeff2*dM2);
    if(cost2 > maxCost2) continue;
    if(cost2 < (*minCost2)) {
      (*minCost2) = cost2;
      mintp1 = tp1;
    }
  }
  return mintp1;
}

TrackPoint *SMS :: nearestForward2(TrackPoint **begin, TrackPoint *tp0, float *minCost2, float maxCost2, float maxDF, float dMCoeff2, float dNCoeff2) 
{
  *minCost2 = TrackPointNoCont;
  float minF = tp0->f - maxDF;
  float maxF = tp0->f + maxDF;
  float maxDF2 = square(maxDF);
  while((*begin) && (*begin)->f < minF) {
    (*begin) = (*begin)->pn;
  }
  TrackPoint *mintp1 = NULL;
  for(TrackPoint *tp1 = (*begin);
      tp1;
      tp1 = tp1->pn) {
    if(!tp1->owner) continue;
    float df2 = square(tp1->f - tp0->f);
    if(df2 > maxDF2) break;
    float dM2 = dBApprox(0.25f*tp1->m2,tp0->m2);
    float cost2 = (df2+dMCoeff2*dM2);
    if(cost2 > maxCost2) continue;
    if(cost2 < (*minCost2)) {
      (*minCost2) = cost2;
      mintp1 = tp1;
    }
  }
  return mintp1;
}

TrackPoint *SMS :: nearestReverse2(TrackPoint **begin, TrackPoint *tp0, float *minCost2, float maxCost2, float maxDF, float dMCoeff2, float dNCoeff2) 
{
  *minCost2 = TrackPointNoCont;
  float minF = tp0->f - maxDF;
  float maxF = tp0->f + maxDF;
  float maxDF2 = square(maxDF);
  while((*begin) && (*begin)->f > maxF) {
    (*begin) = (*begin)->pp;
  }
  TrackPoint *mintp1 = NULL;
  for(TrackPoint *tp1 = (*begin);
      tp1;
      tp1 = tp1->pp) {
    if(!tp1->owner) continue;
    float df2 = square(tp1->f - tp0->f);
    if(df2 > maxDF2) break;
    float dM2 = dBApprox(tp1->m2,tp0->m2);
    float cost2 = (df2+dMCoeff2*dM2);
    if(cost2 > maxCost2) continue;
    if(cost2 < (*minCost2)) {
      (*minCost2) = cost2;
      mintp1 = tp1;
    }
  }
  return mintp1;
}

void SMS :: connect(TrackPoint *tp0, TrackPoint *tp1, int ilo, int c)
{
  TimeType time = assigntime[c];
  if(tp0->slice->band == tp1->slice->band) {
#ifdef MULTITHREADED
    pthread_mutex_lock(&trackMutex[c]);
#endif    
    tp0->owner->push_back(tp1);
#ifdef MULTITHREADED
    pthread_mutex_unlock(&trackMutex[c]);
#endif
  } else if(tp0->slice->band < tp1->slice->band) {
    Track *precursor = tp0->owner;
    if(ilo == 1) {
#ifdef MULTITHREADED
      pthread_mutex_lock(&trackMutex[c]);
#endif
      precursor->push_back(tp1);
      precursor->endTrack(true);
      TimeType time = precursor->end/res;
#ifdef MULTITHREADED
      pthread_mutex_unlock(&trackMutex[c]);
#endif
#ifdef MULTITHREADED
      pthread_mutex_lock(&lo->trackMutex[c]);
#endif
      lo->createTrack(c,tp1,time,true);
#ifdef MULTITHREADED
      pthread_mutex_unlock(&lo->trackMutex[c]);
#endif
    } else {
#ifdef MULTITHREADED
      pthread_mutex_lock(&trackMutex[c]);
#endif
      TimeType time = precursor->end/res;
      precursor->endTrack(true);
      TrackPoint *last = precursor->back();
#ifdef MULTITHREADED
      pthread_mutex_unlock(&trackMutex[c]);
#endif
#ifdef MULTITHREADED
      pthread_mutex_lock(&lo->trackMutex[c]);
#endif
      Track *t = lo->createTrack(c,last,time,true);
      t->push_back(tp1);
#ifdef MULTITHREADED
      pthread_mutex_unlock(&lo->trackMutex[c]);
#endif
      last->owner = precursor;
    }
  } else {
    Track *precursor = tp0->owner;
#ifdef MULTITHREADED
    pthread_mutex_lock(&trackMutex[c]);
#endif
    precursor->push_back(tp1);
    precursor->endTrack(true);
    TimeType time = precursor->end*hi->res;
#ifdef MULTITHREADED
    pthread_mutex_unlock(&trackMutex[c]);
#endif
#ifdef MULTITHREADED
    pthread_mutex_lock(&hi->trackMutex[c]);
#endif
    hi->createTrack(c,tp1,time,true);
#ifdef MULTITHREADED
    pthread_mutex_unlock(&hi->trackMutex[c]);
#endif
  }
  tp0->bConnected = true;
  tp1->bConnected = true;
  tp0->bOwned = true;
  tp1->bOwned = true;
  if(tp0->dupcont) {
    TrackPoint *dup = tp0->dupcont;
    if(!dup->owner) {
      dup->bOwned = true;
      dup->bDelete = true;
    }
  }
  TrackPoint *dup2 = tp0->dup[2];
  if(dup2 && dup2 != tp1 && !dup2->owner) {
    dup2->bOwned = true;
    dup2->bDelete = true;
  }
  for(int d=0;d<3;d++) {
    TrackPoint *dup = tp1->dup[d];
    if(dup && !dup->owner && (d<2 || dup->slice->band < tp1->slice->band)) {
      dup->bOwned = true;
      dup->bDelete = true;
    }
  }
}

void SMS :: mark(long offset, int c)
{
  mark(offset,0,c);
  if(offset&resMask) {
    mark(offset,1,c);
  }
}

void SMS :: mark(long offset, long offsetlo, int c)
{
  if(!lo) return;
#ifdef MULTITHREADED
  pthread_mutex_lock(&lo->sliceMutex[c]);
#endif
  Slice *sliceL1 = lo->sliceBuffer[c].read(lo->sliceBuffer[c].readPos+offset/res+offsetlo);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&lo->sliceMutex[c]);
#endif
#ifdef MULTITHREADED
  pthread_mutex_lock(&sliceMutex[c]);
#endif
  Slice *sliceM1 = sliceBuffer[c].read(sliceBuffer[c].readPos+offset);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&sliceMutex[c]);
#endif
  bool b0 = !(offset&resMask);
  bool bDone = false;
  bool bLastDitch = false;
  while(!bDone) {
    int nToCont = 0;
    int nCont = 0;
    TrackPoint *rbegin = NULL;
    TrackPoint *begin = sliceL1->bottom;
    for(TrackPoint *tp = sliceM1->bottom;
        tp;
        tp = tp->pn) {
      if(tp->bMarked) continue;
      if(tp->f > maxFMatchM) {
        break;
      } else {
        rbegin = tp;
      }
      float F;
      tp->cont = nearestForward(&begin,tp,&F,maxCost2Match,maxDFMatch,dMCoeff2Match);
      if(tp->cont) nToCont++;
    }
    if(sliceL1) {
      for(TrackPoint *tp = sliceL1->top;
          tp;
          tp = tp->pp) {
        if(tp->f < minFLo) break;
        float F;
        tp->cont = nearestReverse(&rbegin,tp,&F,maxCost2Match,maxDFMatch,dMCoeff2Match);
      }
    }
    for(TrackPoint *tp0 = sliceM1->bottom;
        tp0;
        tp0 = tp0->pn) {
      if(tp0->bMarked) continue;
      if(tp0->f > maxFMatchM) {
        break;
      }
      TrackPoint *tp1 = tp0->cont;
      if(tp1) {
        if(bLastDitch || tp1->cont == tp0) {
          nCont++;
          bool bAlreadyMarked = false;
          if(b0) {
            if(tp1->dup[1] || tp0->dup[1]) {
              bAlreadyMarked = true;
            }
          } else {
            if(tp1->dup[2-2*offsetlo] || tp0->dup[2*offsetlo]) {
              bAlreadyMarked = true;
            }
          }
          if(!bAlreadyMarked) {
            if(b0) {
              tp1->dup[1] = tp0;
              tp0->dup[1] = tp1;
            } else {
              tp1->dup[2-2*offsetlo] = tp0;
              tp0->dup[2*offsetlo] = tp1;
            }
          }
          tp0->bMarked = true;
        }
      }
    }
    bDone = (nToCont == nCont);
    bLastDitch = (!bDone && nCont==0);
  }
}

void SMS :: assignStart(long offset, int c)
{
  bAssignDone[c] = false;
#ifdef MULTITHREADED
  pthread_mutex_lock(&sliceMutex[c]);
#endif
  sliceM0[c] = sliceBuffer[c].read(sliceBuffer[c].readPos+offset);
  sliceM1[c] = sliceBuffer[c].read(sliceBuffer[c].readPos+offset+1);
  if(res == 2) {
    sliceM2[c] = sliceBuffer[c].read(sliceBuffer[c].readPos+offset+2);
  } else {
    sliceM2[c] = NULL;
  }
#ifdef MULTITHREADED
  pthread_mutex_unlock(&sliceMutex[c]);	
#endif
  for(TrackPoint *tp = sliceM0[c]->bottom;
      tp;
      tp = tp->pn) {
    if(!tp->owner->bEnded) {
      tp->owner->bEnd = true;
      tp->bConnected = false;
      tp->bOwned = false;
    } else {
      tp->bConnected = true;
      tp->bOwned = true;
    }
  }
#ifdef MULTITHREADED
  if(hi) pthread_mutex_lock(&hi->sliceMutex[c]);
#endif
  sliceH0[c] = hi?hi->sliceBuffer[c].read(hi->sliceBuffer[c].readPos+(offset+1)*hi->res):NULL;
  sliceH0[c] = NULL;
  sliceH1[c] = hi?hi->sliceBuffer[c].read(hi->sliceBuffer[c].readPos+(offset+1)*hi->res):NULL;  
#ifdef MULTITHREADED 
  if(hi) pthread_mutex_unlock(&hi->sliceMutex[c]);
#endif
#ifdef MULTITHREADED
  if(lo) pthread_mutex_lock(&lo->sliceMutex[c]);
#endif
  sliceL0[c] = lo?lo->sliceBuffer[c].read(lo->sliceBuffer[c].readPos+offset/res+1):NULL;
  sliceL0[c] = NULL;
  sliceL1[c] = lo?lo->sliceBuffer[c].read(lo->sliceBuffer[c].readPos+offset/res+1):NULL;
#ifdef MULTITHREADED
	if(lo) pthread_mutex_unlock(&lo->sliceMutex[c]);
#endif
}

void SMS :: assignInit(long offset, int c)
{
  for(TrackPoint *tp = sliceM1[c]->bottom;
      tp;
      tp = tp->pn) {
    tp->cont = NULL;
    tp->contF = TrackPointNoCont;
  }
  if(sliceM2[c]) {
    for(TrackPoint *tp = sliceM2[c]->bottom;
        tp;
        tp = tp->pn) {
      tp->cont = NULL;
      tp->contF = TrackPointNoCont;
    }
  }
}

void SMS :: assignFind(long offset, int c)
{
  if(bAssignDone[c]) return;
  Slice *sliceM0 = this->sliceM0[c];
  Slice *sliceM1 = this->sliceM1[c];
  Slice *sliceM2 = this->sliceM2[c];
  Slice *sliceL1 = this->sliceL1[c];
  Slice *sliceH1 = this->sliceH1[c];
  TrackPoint *begin;
  begin = sliceM0->bottom;
  for(TrackPoint *tp = sliceM1->bottom;
      tp;
      tp = tp->pn) {
    if(tp->bOwned) continue;
    float F;	
    tp->bConnect = false;
    TrackPoint *minM = nearestForward(&begin,tp,&F,maxCost2,maxDF,dMCoeff2,dNCoeff2);
    if(minM && F < tp->contF) {
      tp->cont = minM;
      tp->contF = F;
    }
  }
  if(sliceL1) {
    TrackPoint *rbegin = sliceM0->top;
    for(TrackPoint *tp = sliceL1->top;
        tp;
        tp = tp->pp) {
      if(tp->bOwned) continue;
      if(tp->f < minFLo) break;
      float F;
      TrackPoint *minL = nearestReverse(&rbegin,tp,&F,maxCost2,maxDF,dMCoeff2,dNCoeff2);
      if(minL) {
        F *= localFavorRatio;
        if(F < tp->contF) {
          tp->cont = minL;
          tp->contF = F;
        }
      }
    }
  }
  begin = sliceM0->bottom;
  if(sliceH1) {
    for(TrackPoint *tp = sliceH1->bottom;
        tp;
        tp = tp->pn) {
      if(tp->bOwned) continue;
      if(tp->f > maxFHi) break;
      float F;
      TrackPoint *minH = nearestForward(&begin,tp,&F,maxCost2,maxDF,dMCoeff2,dNCoeff2);
      if(minH) {
        F *= localFavorRatio;
        if(F < tp->contF) {
          tp->cont = minH;
          tp->contF = F;
        }
      }
    }
  }
  if(sliceM2 && !(offset&resMask)) {
    begin = sliceM1->bottom;
    for(TrackPoint *tp = sliceM2->bottom;
        tp;
        tp = tp->pn) {
      if(tp->bOwned) continue;
      float F;	
      tp->bConnect = false;
      TrackPoint *minM = nearestForward(&begin,tp,&F,maxCost2,maxDF,dMCoeff2);
      if(minM) {
        tp->cont = minM;
        tp->contF = F;
      }
    }
    if(sliceL1) {
      for(TrackPoint *tp = sliceM2->bottom;
          tp;
          tp = tp->pn) {
        if(tp->bOwned) continue;
        if(tp->f > maxFMid) break;
        float F;	
        TrackPoint *rbegin = sliceL1->top;
        TrackPoint *minL = nearestReverse(&rbegin,tp,&F,maxCost2,maxDF,dMCoeff2);
        if(minL) {
          F *= localFavorRatio;
          if(F < tp->contF) {
            tp->cont = minL;
            tp->contF = F;
          }
        }
      }
    }
  }
}

bool SMS :: assignConnect(long offset, int c, bool bLastDitch)
{
  if(bAssignDone[c]) return false;
  Slice *sliceM0 = this->sliceM0[c];
  Slice *sliceM1 = this->sliceM1[c];
  Slice *sliceL1 = this->sliceL1[c];
  Slice *sliceH1 = this->sliceH1[c];
  int nToCont = 0;
  int nCont = 0;
  bool b0 = !(offset&resMask);
  int ilo;
  if(res == 2 && b0) {
    ilo = 0;
  } else {
    ilo = 1;
  }
  TrackPoint *beginM1 = sliceM1->bottom;
  TrackPoint *beginH1;
  if(sliceH1) beginH1 = sliceH1->bottom;
  for(TrackPoint *tp = sliceM0->bottom;
      tp;
      tp = tp->pn) {
    if(tp->bOwned) continue;
    float FM1 = TrackPointNoCont;
    float FL1 = TrackPointNoCont;
    float FH1 = TrackPointNoCont;
    TrackPoint *minM1 = nearestForward(&beginM1,tp,&FM1,maxCost2,maxDF,dMCoeff2,dNCoeff2);
    TrackPoint *minL1 = NULL;
    if(sliceL1 && tp->f < maxFMid) {
      TrackPoint *rbeginL1 = sliceL1->top;
      minL1 = nearestReverse(&rbeginL1,tp,&FL1,maxCost2,maxDF,dMCoeff2,dNCoeff2);
      FL1 *= localFavorRatio;
    }
    TrackPoint *minH1 = NULL;
    if(sliceH1 && tp->f > minFMid) {
      minH1 = nearestForward(&beginH1,tp,&FH1,maxCost2,maxDF,dMCoeff2,dNCoeff2);
      FH1 *= localFavorRatio;
    }
    if(minM1 &&
       ((FM1<=FH1 && FM1<=FL1)
        ||(minL1 && FL1<=FH1 && FL1<=FM1 && minL1->dup[ilo] == minM1)
        ||(minH1 && FH1<=FL1 && FH1<=FM1 && minH1->dup[1] == minM1))) {
      if(ilo == 1 && minL1 && minL1->dup[1] == minM1) {
        tp->dupcont = minL1;
      } else if(minH1 && minH1->dup[1] == minM1) {
        tp->dupcont = minH1;
      } else {
        tp->dupcont = NULL;
      }
      tp->contF = FM1;
      tp->cont = minM1;
      nToCont++;
    } else if(minL1 && FL1<=FM1 && FL1<=FH1) {
      if(minM1 && minL1->dup[ilo] == minM1) {
        tp->dupcont = minM1;
      } else {
        tp->dupcont = NULL;
      }
      tp->contF = FL1;
      tp->cont = minL1;
      nToCont++;
    } else if(minH1 && FH1<=FM1 && FH1<=FL1) {
      if(minM1 && minH1->dup[1] == minM1) {
        tp->dupcont = minM1;
      } else {
        tp->dupcont = NULL;
      }
      tp->contF = FH1;
      tp->cont = minH1;
      nToCont++;
    } else {
      tp->cont = NULL;
    }
  }
  for(TrackPoint *tp0 = sliceM0->bottom;
      tp0;
      tp0 = tp0->pn) {
    if(tp0->bOwned) continue;
    tp0->bConnect = false;
    TrackPoint *tp1 = tp0->cont;
    TimeType time = assigntime[c];
    if(tp1 && !tp1->bOwned &&
       (bLastDitch ||
        (tp1->cont == tp0) ||
        ((tp1->cont && tp0->contF <= tp1->cont->contF) &&
         ((tp1->cont->dup[0] == tp0) ||
          (tp1->cont->dup[1] == tp0))))) {
      tp1->cont = tp0;
      tp0->bConnect = true;
      tp1->bConnect = true;
    }
  }
  for(TrackPoint *tp0 = sliceM0->bottom;
      tp0;
      tp0 = tp0->pn) {
    if(tp0->bOwned) continue;
    TrackPoint *tp1 = tp0->cont;
    if(tp0->bConnect && tp1 && !tp1->bOwned && tp1->bConnect && tp1->cont == tp0) {
      TrackPoint *dupcont = tp0->dupcont;
      if(dupcont && dupcont->bConnect) {
        if(!tp1->bConnected && !dupcont->bConnected) {
          if(!tp0->bConnected && (dupcont->cont == NULL || tp0->contF <= dupcont->cont->contF)) {
            nCont++;
            connect(tp0,tp1,ilo,c);
            tp0->owner->bEnd = false;
            dupcont->bConnect = false;
          } else if(dupcont->cont && !dupcont->cont->bConnected) {
            nCont++;
            connect(dupcont->cont,dupcont,ilo,c);
            dupcont->cont->owner->bEnd = false;
            tp1->bConnect = false;
          }
        }
      } else if(!tp0->bConnected && !tp1->bConnected) {
        nCont++;
        connect(tp0,tp1,ilo,c);
        tp0->owner->bEnd = false;
      }
    }
  }
  bAssignDone[c] = (nToCont == nCont || bLastDitch);
  return !(bAssignDone[c] || nCont == 0);
}

void SMS :: start(long offset, int c)
{
  started[c].clear();
  ended[c].clear();
#ifdef MULTITHREADED
  pthread_mutex_lock(&trackMutex[c]);
#endif
  for(list<Track*>::iterator tt = assignTracks[c].begin(); 
      tt != assignTracks[c].end(); ) {
    Track *t = (*tt);
    bool bKeep;  
    if(t->bEnded) {
      bKeep = ((!t->bRender) && (t->bStitch || t->size() >= minTrackSize));
      if(assigntime[c] > t->last) {
        returnTrackIndex(c,t);
        list<Track*>::iterator eraseMe = tt;
        ++tt;
        assignTracks[c].erase(eraseMe);
      } else {
        ++tt;
      }
    } else if(t->bEnd) {
      bKeep = (t->bStitch || t->size() >= minTrackSize);
      if(bKeep) {
        bKeep = !t->bRender;
        t->endTrack(false);
        ended[c].push_back(t->back());
        ++tt;
      } else {
        list<Track*>::iterator eraseMe = tt;
        ++tt;
        assignTracks[c].erase(eraseMe);
        returnTrackIndex(c,t);
        t->absorb();
        delete t;
        continue;
      }
    } else {
      bKeep = ((!t->bRender) && (t->bStitch || t->size() >= minTrackSize));
      ++tt;
    }
    if(bKeep) {
      list<Track*>::reverse_iterator tt0 = renderTracks[c].rbegin();
      while(tt0 != renderTracks[c].rend()) {
        Track *t0 = *tt0;
        if(t->start >= t0->start) {
          break;
        }
        tt0++;
      }
      renderTracks[c].insert(tt0.base(),t);        
      t->bRender = true;
    }
  }
#ifdef MULTITHREADED
  pthread_mutex_unlock(&trackMutex[c]);
#endif
#ifdef MULTITHREADED
  pthread_mutex_lock(&sliceMutex[c]);
#endif
  Slice *sliceM0 = sliceBuffer[c].read(sliceBuffer[c].readPos+offset);
  adjust2SliceQueue[c].push(sliceM0);
  adjust1SliceQueue[c].push(sliceM0);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&sliceMutex[c]);	
#endif
  for(TrackPoint *tp = sliceM0->bottom;
      tp;) {
    TrackPoint *tpn = tp->pn;
    if(tp->bOwned) {
      if(tp->bDelete) {
        tp->destroy();
      }
    } else {
      Track *t = createTrack(c,tp,assigntime[c],false);
      started[c].push_back(tp);
      for(int d=0;d<2;d++) {
        TrackPoint *dup = tp->dup[d];
        if(dup && !dup->owner) {
          dup->destroy();
        }
      }
    }
    tp = tpn;
  }
  assigntime[c]++;
}

void SMS :: splitMerge(int c)
{
  TimeType time = assigntime[c] - 1;
#ifdef MULTITHREADED
  pthread_mutex_lock(&trackMutex[c]);
#endif
  Slice *sliceM0 = this->sliceM0[c];
  Slice *sliceL0 = this->sliceL0[c];
  Slice *sliceH0 = this->sliceH0[c];
  Slice *sliceM1 = this->sliceM1[c];
  Slice *sliceL1 = this->sliceL1[c];
  Slice *sliceH1 = this->sliceH1[c];
  TrackPoint *rbeginL0 = sliceL0?sliceL0->top:NULL;
  TrackPoint *beginM0 = sliceM0->bottom;
  TrackPoint *beginH0 = sliceH0?sliceH0->bottom:NULL;
  for(list<TrackPoint*>::iterator i = started[c].begin();
      i != started[c].end();
      ++i) {
    TrackPoint *tp = *i;
    float F, FL, FH;	
    tp->cont = nearestForward2(&beginM0,tp,&F,maxCost2SplitMerge,maxDFSplitMerge,dMCoeff2SplitMerge);
    TrackPoint *minL = nearestReverse2(&rbeginL0,tp,&FL,maxCost2SplitMerge,maxDFSplitMerge,dMCoeff2SplitMerge);
    if(minL) {
      FL *= localFavorRatio;
      if(FL < F) {
        tp->cont = minL;
        F = FL;
      }
    }
    TrackPoint *minH = nearestForward2(&beginH0,tp,&FH,maxCost2SplitMerge,maxDFSplitMerge,dMCoeff2SplitMerge);
    if(minH) {
      FH *= localFavorRatio;
      if(FH < F) {
        tp->cont = minH;
      }
    }
    if(tp->cont) {
      tp->owner->point.insert(tp->owner->point.begin(),tp->cont);
      tp->owner->first--;
      tp->owner->bStitch = true;
      tp->bSplit = true;
      tp->cont->bSplit = true;
      tp->owner->bSplit = true;
      tp->cont->refCount++;
      tp->cont->owner->bStitch = true;
    }
  }

  TrackPoint *rbeginL1 = sliceL1?sliceL1->top:NULL;
  TrackPoint *beginM1 = sliceM1->bottom;
  TrackPoint *beginH1 = sliceH1?sliceH1->bottom:NULL;
  for(list<TrackPoint*>::iterator i = ended[c].begin();
      i != ended[c].end();
      ++i) {
    TrackPoint *tp = *i;
    float F, FL, FH;
    tp->cont = nearestForward2(&beginM1,tp,&F,maxCost2SplitMerge,maxDFSplitMerge,dMCoeff2SplitMerge);
    TrackPoint *minL = nearestReverse2(&rbeginL1,tp,&FL,maxCost2SplitMerge,maxDFSplitMerge,dMCoeff2SplitMerge);
    if(minL) {
      FL *= localFavorRatio;
      if(FL < F) {
        tp->cont = minL;
        F = FL;
      }
    }
    TrackPoint *minH = nearestForward2(&beginH1,tp,&FH,maxCost2SplitMerge,maxDFSplitMerge,dMCoeff2SplitMerge);
    if(minH) {
      FH *= localFavorRatio;
      if(FH < F) {
        tp->cont = minH;
      }
    }
    if(tp->cont) {
      tp->owner->point.insert(tp->owner->point.end(),tp->cont);
      tp->owner->last++;
      tp->owner->bStitch = true;
      tp->bMerge = true;
      tp->cont->bMerge = true;
      tp->owner->bMerge = true;
      tp->cont->refCount++;
      tp->cont->owner->bStitch = true;
    }
  }
#ifdef MULTITHREADED
  pthread_mutex_unlock(&trackMutex[c]);
#endif
}

void SMS :: advance(int c)
{
#ifdef MULTITHREADED
  pthread_mutex_lock(&sliceMutex[c]);
#endif
  sliceBuffer[c].advance(1);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&sliceMutex[c]);
#endif
}

void SMS :: add(grain *g0, grain *g1, grain *g2, int c)
{
  if(c == 0) {
    if(band >= minTrial1Band) {
      c2even(g0->x, x00[0], N);
    }
    if(band >= minTrial2Band) {
      c2even(g1->x, x10[0], N);
    }
    c2even(g2->x, x2[0], N);
  } else {
    if(band >= minTrial1Band) {
      c2odd(g0->x, x00[1], N);
    }
    if(band >= minTrial2Band) {
      c2odd(g1->x, x10[1], N);
    }
    c2odd(g2->x, x2[1], N);
  }

  float *mag0;
  if(band >= minTrial1Band) {
    mag0 = (float*)malloc((Nover2+1)*sizeof(float));
    calcmags(mag0, x00[c]);
  }
  float *mag1;
  if(band >= minTrial2Band) {
    mag1 = (float*)malloc((Nover2+1)*sizeof(float));
    calcmags(mag1, x10[c]);
  }
  float mag2sum[1024];
  memset(mag2sum,0,1024*sizeof(float));

  float *mag2 = this->mag2[c];
  calcmags(mag2sum, g2->x);
  calcmags(mag2sum, x2[c]);
  calcmags(mag2, x2[c]);
#ifdef MULTITHREADED
  pthread_mutex_lock(&magMutex[c]);
#endif
  if(band >= minTrial1Band) mag0Queue[c].push(mag0);
  if(band >= minTrial2Band) mag1Queue[c].push(mag1);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&magMutex[c]);
#endif
  float magmax = mag2[0];
  for(int k=1;k<=kEnd;k++) {
    if(magmax < mag2[k]) magmax = mag2[k];
  }
  float peakmin = magmax * peakThresh;

  float xt2 = 1.0f;
  bool bTroughN1 = false;
  bool bTroughN2 = false;
  float x0 = 1.0f;
  float y0 = mag2[1];
  float x1 = 0.0f;
  float y1 = 0.0f;
  bool bX0 = !lo;
  bool bX1 = false;
  TrackPoint *prev = NULL;

  Slice *slice = new Slice(band,addtime[c]);

  for(int k=1; k<=kEnd; k++) {
    if(mag2[k] > peakmin && mag2[k] > mag2[k-1] && mag2[k] >= mag2[k+1]) {
      if(k < kLo) {
        x0 = findExtremum(mag2,mag2,k,&y0);
        bX0 = true;
      } else if(k > kHi) {
        if(!bX1) {
          x1 = findExtremum(mag2,mag2,k,&y1);
          if(prev) {
            prev->x01 = x1;
            prev->y01 = y1;
          }
          bX1 = true;
        }
      } else {
        TrackPoint *p = new TrackPoint(slice,peak2N,x2[c],mag2,mag2,k,N,band);

        if(prev) {
          prev->pn = p;
          p->pp = prev;
        } else {
          slice->bottom = p;
        }
        slice->top = p;
        prev = p;
        p->xtn2 = (float)maxK;
        bTroughN1 = true;
        bTroughN2 = true;
        p->xtp2 = xt2;
        p->x01 = x0;
        p->y01 = y0;
      }
    } else if(mag2[k] <= mag2[k-1] && mag2[k] <= mag2[k+1]) {
      xt2 = findExtremum(mag2,mag2,k,NULL);
      xt2 = max(1.0f,xt2);
      xt2 = min((float)kEnd,xt2);
      if(bTroughN2) {
        prev->xtn2 = xt2;
        bTroughN2 = false;
      }
    }
  }
  if(bTroughN2) {
    prev->xtn2 = (float)kEnd;
  }
  if(!bX1 && !hi) {
    x1 = (float)kEnd;
    y1 = mag2[kEnd];
    bX1 = true;
  }
  float *dec2 = this->dec2[c];
  memset(dec2,0,(Nover2+1)*sizeof(float));
  if(bX0 && prev) {
    int k1 = lrintf(x0);
    int ko1 = k1 > x0 ? -1 : 1;
    float kf1 = k1 > x0 ? k1 - x0 : x0 - k1; 
    int k3 = min(kEnd,k1+peakWidth2);
    for(int k=lrintf(slice->bottom->xtp2);k<=k3;k++) {
      float m = interp2(k-k1,ko1,kf1);
      dec2[k] += m * y0;
    }
  }
  if(bX1 && prev) {
    int k1 = lrintf(x1);
    int ko1 = k1 > x1 ? -1 : 1;
    float kf1 = k1 > x1 ? k1 - x1 : x1 - k1; 
    int k3 = lrintf(slice->top->xtn2);
    for(int k=max(0,k1-peakWidth2);k<=k3;k++) {
      float m = interp2(k-k1,ko1,kf1);
      dec2[k] += m * y1;
    }
  }
  for(TrackPoint *p = slice->bottom;
      p;
      p = p->pn) {
    int k1 = lrintf(p->x);
    int ko1 = k1 > p->x ? -1 : 1;
    float kf1 = k1 > p->x ? k1 - p->x : p->x - k1; 
    int k0 = lrintf(p->xtp2);
    float kf0 = (k0 > p->xtp2 ? k0 - p->xtp2 : p->xtp2 - k0);
    int k2 = lrintf(p->xtn2);
    float kf2 = (k2 > p->xtn2 ? k2 - p->xtn2 : p->xtn2 - k2);
    float m2 = 0.0f;
    if(k0 < p->xtp2) {
      m2 += (mag2[k0] + mag2[k0+1]) * 0.5f * (1.0f - kf0) + 0.5f * mag2[k0+1];
      int i = k0 - k1;
      float m = interp2(i,ko1,kf1) * p->y;
      m = min(m,mag2[k0]) * 0.5f * (1.0f + kf0);
      m2 += m;
      dec2[k0] += m;
      m = interp2(i+1,ko1,kf1) * p->y;
      m = min(m,mag2[k0+1]) * 0.5f * kf0;
      m2 += m;
      dec2[k0+1] += m;
      m = interp2(i-1,ko1,kf1) * p->y;
      m = min(m,mag2[k0-1]);
      m2 += m;
      dec2[k0-1] += m;
    } else {
      m2 += (mag2[k0] + mag2[k0-1]) * 0.5f * kf0 + 0.5f * mag2[k0] + mag2[k0+1];
      int i = k0 - k1;
      float m = interp2(i,ko1,kf1) * p->y;
      m = min(m,mag2[k0]) * 0.5f * (1.0f - kf0);
      m2 += m;
      dec2[k0] += m;
      m = interp2(i-1,ko1,kf1) * p->y;
      m = min(m,mag2[k0-1]) * 0.5f * (2.0f - kf0);
      m2 += m;
      dec2[k0-1] += m;
    }
    if(k2 < p->xtn2) {
      m2 += mag2[k2-1] + 0.5f * mag2[k2] + 0.5f * kf2 * (mag2[k2] + mag2[k2+1]);
      int i = k2 - k1;
      float m = interp2(i,ko1,kf1) * p->y;
      m = min(m,mag2[k2]) * 0.5f * (1.0f - kf2);
      m2 += m;
      dec2[k2] += m;
      m = interp2(i+1,ko1,kf1) * p->y;
      m = min(m,mag2[k2+1]) * 0.5f * (2.0f - kf2);
      m2 += m;
      dec2[k2+1] += m;
    } else {
      m2 += (mag2[k2-1] + mag2[k2]) * (1.0f - kf2) * 0.5f + 0.5f * mag2[k2-1];
      int i = k2 - k1;
      float m = interp2(i,ko1,kf1) * p->y;
      m = min(m,mag2[k2]) * 0.5f * (1.0f + kf2);
      m2 += m;
      dec2[k2] += m;
      m = interp2(i-1,ko1,kf1) * p->y;
      m = min(m,mag2[k2-1]) * 0.5f * kf2;
      m2 += m;
      dec2[k2-1] += m;
      m = interp2(i+1,ko1,kf1) * p->y;
      m = min(m,mag2[k2+1]);
      m2 += m;
      dec2[k2+1] += m;
    }
    for(int k=k0+2;k<k2-1;k++) {
      m2 += mag2[k];
    }
    if(k0 + 1 == k2 - 1) {
      m2 -= mag2[k0+1];
    }
    for(int k=max(0,k1-peakWidth2);k<k0-1;k++) {
      float m = interp2(k-k1,ko1,kf1) * p->y;
      m = min(m,mag2[k]);
      m2 += m;
      dec2[k] += m;
    }
    int k3 = min(kEnd,k1+peakWidth2);
    for(int k=k2+2;k<=k3;k++) {
      float m = interp2(k-k1,ko1,kf1) * p->y;
      m = min(m,mag2[k]);
      m2 += m;
      dec2[k] += m;
    }
    p->m2 = m2;
  }
  float m2max = 0.0f;
  for(TrackPoint *p = slice->bottom;
      p;
      p = p->pn) {
    int k1 = lrintf(p->x);
    int ko1 = k1 > p->x ? -1 : 1;
    float kf1 = k1 > p->x ? k1 - p->x : p->x - k1; 
    int k0 = lrintf(p->xtp2);
    float kf0 = (k0 > p->xtp2 ? k0 - p->xtp2 : p->xtp2 - k0);
    int k2 = lrintf(p->xtn2);
    float kf2 = (k2 > p->xtn2 ? k2 - p->xtn2 : p->xtn2 - k2);
    float mdec = 0.0f;
    if(k0 < p->xtp2) {
      mdec += (dec2[k0] + dec2[k0+1]) * 0.5f * (1.0f - kf0) + 0.5f * dec2[k0+1];
    } else {
      mdec += (dec2[k0] + dec2[k0-1]) * 0.5f * kf0 + 0.5f * dec2[k0] + dec2[k0+1];
    }
    if(k2 < p->xtn2) {
      mdec += dec2[k2-1] + 0.5f * dec2[k2] + 0.5f * kf2 * (dec2[k2] + dec2[k2+1]);
    } else {
      mdec += (dec2[k2-1] + dec2[k2]) * (1.0f - kf2) * 0.5f + 0.5f * dec2[k2-1];
    }
    for(int k=k0+2;k<k2-1;k++) {
      mdec += dec2[k];
    }
    if(k0 + 1 == k2 - 1) {
      mdec -= dec2[k0+1];
    }

    p->m2 -= mdec;
    p->m2 *= mNorm;
    if(p->m2 > m2max) {
      m2max = p->m2;
    }
  }
  float m2min = m2max * peakThresh;
  for(TrackPoint *p = slice->bottom;
      p; ) {
    TrackPoint *pn = p->pn;
    if(p->m2 < m2min) {
      if(p->m2 < 0) { p->m2 = 0; }
      p->absorb();
      delete p;
    }
    p = pn;
  }
    
#ifdef MULTITHREADED
  pthread_mutex_lock(&sliceMutex[c]);
#endif
  sliceBuffer[c].write(slice);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&sliceMutex[c]);
#endif
  addtime[c]++;
}

void SMS :: prepad1(audio *buf, long n)
{
  if(band >= minTrial2Band) {
    trial2GrainBuf->write(buf,n);
  }
}

void SMS :: prepad0(audio *buf, long n)
{
  if(band >= minTrial1Band) {
    trial1GrainBuf->write(buf,n);
  }
}

int SMS :: getTrial2Latency()
{ 
  return minTrackSize;
}

Track *SMS :: createTrack(int c, TrackPoint *tp, const TimeType &time, bool bStitch) 
{
  TrackIndexType index;
  if(trackIndex[c].empty()) {
    index = trackIndexNone;
  } else {
    index = trackIndex[c].front();
    trackIndex[c].pop();
  }
  Track *t = new Track(h1,index,tp,time,bStitch);
  assignTracks[c].push_back(t);
  return t;
}

void SMS :: returnTrackIndex(int c, Track *t)
{
  if(t->index != trackIndexNone) {
    trackIndex[c].push(t->index);
    t->index = trackIndexNone;
  }
}

float SMS :: interp2(int k, int ko, float kf) {
  return (1.0f-kf)*peak2N[k] + kf*peak2N[k+ko];
}

float SMS :: findExtremum(float *mag, float *mag2, int k, float *y)
{
  float y0 = mag[k-1];
  float y1 = mag[k];
  float y2 = mag[k+1];
  float d = (y0 + y2 - y1 - y1);
  float x = (d==0.0f?k:k + 0.5f * (y0 - y2) / d);
  if(y) {
    int ki = lrintf(x);
    float kf = ki<x?x-ki:ki-x;
    int ki1 = ki<k?ki+1:ki-1;
    *y = ((1.0f-kf)*mag2[ki] + kf*mag2[ki1]);
  }
  return x;
}

void SMS :: calcmags(float *mag, audio *x) {
  for(int k=0;k<=Nover2;k++) {
    mag[k] = norm2(x[k]);
  }
}

SynthRenderer :: SynthRenderer(int channels, int h)
{
  this->channels = channels;
  for(int c=0; c<channels; c++) {
    sines[c] = new ArrayRingBuffer<float>(0);
    synthBufLength[c] = h << 4;
    synthBuf[c] = (float*)malloc(synthBufLength[c]*sizeof(float));
  }
#ifdef MULTITHREADED
  pthread_mutex_init(&bufferMutex,NULL);
#endif
}

SynthRenderer :: ~SynthRenderer()
{
  for(int c=0; c<channels; c++) {
    delete sines[c];
    free(synthBuf[c]);
  }
}

void SynthRenderer :: startTime(int c, const TimeType &time, int n)
{
  if(n > synthBufLength[c]) {
    free(synthBuf[c]);
    synthBufLength[c] = n << 1;
    synthBuf[c] = (float*)malloc(synthBufLength[c]*sizeof(float));
  }
  this->n[c] = n;
  this->time[c] = time;
  memset(synthBuf[c],0,n*sizeof(float));
}

void SynthRenderer :: render(int c, SBSMSTrack *t)
{
  ((Track*)t)->synth(synthBuf[c],time[c],n[c],synthModeOutput,c);
}

void SynthRenderer :: endTime(int c)
{
#ifdef MULTITHREADED
  pthread_mutex_lock(&bufferMutex);
#endif
  int n = this->n[c];
  sines[c]->grow(n);
  long j = sines[c]->writePos;
  float *dest = sines[c]->buf;
  float *src = synthBuf[c];
  for(int k=0; k<n; k++) {
    dest[j++] += src[k];
  }
  sines[c]->writePos += n;
#ifdef MULTITHREADED
  pthread_mutex_unlock(&bufferMutex);
#endif
}

long SynthRenderer :: read(audio *out, long n)
{
#ifdef MULTITHREADED
  pthread_mutex_lock(&bufferMutex);
#endif
  n = min(n,sines[0]->nReadable());
  for(int c=1; c<channels; c++) {
    n = min(n,sines[c]->nReadable());
  }
  for(int c=0; c<channels; c++) {
    float *buf = sines[c]->getReadBuf();
    for(int k=0; k<n; k++) {
      out[k][c] = buf[k];
    }
    sines[c]->advance(n);
  }
#ifdef MULTITHREADED
  pthread_mutex_unlock(&bufferMutex);
#endif
  return n;
}

}
