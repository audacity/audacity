#include "real.h"
#include "utils.h"
#include <math.h>
#include "sms.h"
#include "buffer.h"
#include <limits.h>

using namespace std;
namespace _sbsms_ {

#define SMS_BUF_SIZE 4096

sms :: sms(int N, unsigned short M, unsigned short  M_MAX, int res, int latency, real p, real q, real pad, int Nlo, int channels, TrackAllocator *ta, PeakAllocator *pa) 
{
  this->N = N;
  this->M = M;
  Nover2 = N/2;
  this->p = p;
  this->q = q;
  this->res = res;
  this->m = (real)M/(real)M_MAX;
  this->channels = channels;
  this->ta = ta;
  this->pa = pa;
  terminal = (M == M_MAX);

  datasize = sizeof(char) + 6*sizeof(unsigned short);
  pData = (char*)calloc(datasize,sizeof(char));

  // Threshold (relative to max magnitude) for finding peaks
  peakThresh = .000005f;
  // Threshold (in trackpoint units) for starting a track
  startThresh = 8.0f*sqrt(pad/(real)N) * peakThresh;
  // Max figure of merit for continuing a track (see merit)
  maxMerit2 = square(.11f);
  // Max differency in frequency for continuing a track
  maxDF2 = square(.08f);
  // Coefficient for dB magnitude changes in figure of merit for continuing a track
  dMCoeff = 0.01f;
  // Max increase in dB for continuing a track
  maxdBIncr = (12.0f - 6.0f*m);
  // Max difference in dB for stitching two subband tracks together
  maxdBIncrStitch = (12.0f - 6.0f*m);
  // Max figure of merit for matching peaks for band stitching
  maxMerit2Match = square(.08f);
  // Max difference in frequency for matching peaks for band stitching
  maxDF2Match = square(.04f);
  // Coefficient for dB magnitude changes in figure of merit for band stitching
  dMCoeffMatch = 0.02f;
    // Min number of trackpoints in a track for synthesis
  minNpts = 4;
  // Max width of a peak for determining spectral energy
  peakWidth = 2*((round2int(pad*(real)N/96.0f)+1)/2);
  // Beginning and end of band
  if(terminal) kStart = 1; 
  else kStart = N/4-(int)(0.5*round2int((real)peakWidth*(real)N/(real)Nlo)+6);
  if(M==1) kEnd = N/2;
  else kEnd = N/2-peakWidth;
  // Max frequency for matching peaks for band stitching
  maxFMatch = (0.5f*kEnd+5.0f)*TWOPI/(real)N;
  // Min frequency for matching peaks for band stitching
  minFMatch = (2.0f*kStart-10.0f)*TWOPI/(real)N;
  // Min ratio of peak "top" to total peak energy
  minPeakTopRatio = 0.05f;
  // Min ratio of low frequency resolution/high frequency resolution peaks for determining peaks
  // Ratio for favoring continuing a track in local band over stitching to another band
  localFavorRatio = 1.1f;
  // Max ratio of (peak energy given to other peaks) to (peak energy)
  maxDecRatio = 1.5f;

  // Normalization from spectrum energy to peak amplitude
  mNorm = 1.0f/6.0f * pad * square(8.0f / (real)(N));

  // max magnitude (for determining peaks)
  magmax = 0;

  assigntime[0] = 0;
  assigntime[1] = 0;
  marktime[0] = 0;
  marktime[1] = 0;
  currtime = 0;
  synthtime = 0;

  trax = (list<track*>**)calloc(2,sizeof(list<track*>*));
  trax[0] = new list<track*>;
  trax[1] = new list<track*>;
  peak1 = (real*)calloc(2*N,sizeof(real));
  dec = (real*)calloc(N,sizeof(real));
  mag0[0] = (real*)calloc(N,sizeof(real));
  mag0[1] = (real*)calloc(N,sizeof(real));
  mag1[0] = (real*)calloc(N,sizeof(real));
  mag1[1] = (real*)calloc(N,sizeof(real));
  mag2[0] = (real*)calloc(N,sizeof(real));
  mag2[1] = (real*)calloc(N,sizeof(real));
  trackPointListBuffer[0] = new TrackPointListBuffer();
  trackPointListBuffer[1] = new TrackPointListBuffer();

  h2cum = 0.0f;
  sines2 = new SampleBuf(0);
  outMixer = sines2;
  
  x0[0] = grain :: create(N,1);
  x0[1] = grain :: create(N,1);

  x1[0] = grain :: create(N,1);
  x1[1] = grain :: create(N,1);

  x2[0] = grain :: create(N,1);
  x2[1] = grain :: create(N,1);

  bPeakSet = false;

#ifdef MULTITHREADED
  pthread_mutex_init(&dataMutex,NULL);
  pthread_mutex_init(&tplbMutex[0],NULL);
  pthread_mutex_init(&tplbMutex[1],NULL);
  pthread_mutex_init(&bufferMutex,NULL);
  pthread_mutex_init(&trackMutex[0],NULL);
  pthread_mutex_init(&trackMutex[1],NULL);
#endif
}

sms :: ~sms()
{
  reset();
  free(pData);
  free(mag0[0]);
  free(mag0[1]);
  free(mag1[0]);
  free(mag1[1]);
  free(mag2[0]);
  free(mag2[1]);
  free(dec);
  free(peak1);
  grain :: destroy(x0[0]);
  grain :: destroy(x0[1]);
  grain :: destroy(x1[0]);
  grain :: destroy(x1[1]);
  grain :: destroy(x2[0]);
  grain :: destroy(x2[1]);
  delete sines2;
  for(int c=0;c<channels;c++) {
    for(long k=trackPointListBuffer[c]->readPos; k<trackPointListBuffer[c]->writePos; k++) {
      tplist *tpl = trackPointListBuffer[c]->read(k);
      for(tplist::iterator tpi = tpl->begin();
	  tpi != tpl->end();
	  tpi++) {
	delete (*tpi);
      }
    }
  }
  delete trackPointListBuffer[0];
  delete trackPointListBuffer[1];
  delete trax[0];
  delete trax[1];
  free(trax);
}

void sms :: reset()
{
  assigntime[0] = 0;
  assigntime[1] = 0;
  marktime[0] = 0;
  marktime[1] = 0;
  currtime = 0;
  synthtime = 0;
  h2cum = 0.0f;
  samplePos = 0;

  for(int c=0;c<channels;c++) {
    for(list<track*>::iterator tt=trax[c]->begin(); 
	tt != trax[c]->end(); 
	) {
      track *t = (*tt);      
      list<track*>::iterator eraseMe = tt;
      tt++;
      trax[c]->erase(eraseMe);
      ta->destroy(t);
    }
  }
  
  hSynth.clear();
  sines2->clear();
}

#define LOG_MIN (-16.0f)
#define LOG_MAX (0.0f)

unsigned short sms :: encodeRealLog(real x)
{
  x = log(x);
  if(x<LOG_MIN) return 0;
  else if(x>LOG_MAX) return USHRT_MAX;
  else return (unsigned short)round2int((real)USHRT_MAX*(x-LOG_MIN)/(LOG_MAX-LOG_MIN));
}

real sms :: decodeRealLog(unsigned short x)
{
  return exp(LOG_MIN + (LOG_MAX-LOG_MIN)*(real)x/(real)USHRT_MAX);
}

unsigned short sms :: encodeReal(real x, real min, real max)
{
  if(x<min) return 0;
  else if(x>max) return USHRT_MAX;
  else return (unsigned short)round2int((real)USHRT_MAX*(x-min)/(max-min));
}

real sms :: decodeReal(unsigned short x, real min, real max)
{
  return min + (max-min)*(real)x/(real)USHRT_MAX;
}

void sms :: readTrackPointsFromFile(FILE *fp)
{
  unsigned short h1;
  char *data = pData;
  fread(data,sizeof(unsigned short)+sizeof(char),1,fp);
  h1 = *((unsigned short*)data); data += sizeof(unsigned short);
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  hSynth.write(h1);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
  char ch = *((char*)data);
  channels = ch;
  for(int c=0;c<channels;c++) {
    unsigned short nTracks = 0;
    fread(&nTracks,sizeof(unsigned short),1,fp);
    for(int k=0;k<nTracks;k++) {
      unsigned short index;
      unsigned short precursorIndex;
      char flags;
      bool tailStart;
      bool tailEnd;
      bool bStart;
      bool bEnd;
      tpoint *tp = new tpoint();
      
      char *data = pData;
      fread(data,datasize,1,fp);
      flags = *((char*)data); data += sizeof(char);
      index = *((unsigned short*)data); data += sizeof(unsigned short);
      precursorIndex = *((unsigned short*)data); data += sizeof(unsigned short);
      unsigned short f;
      unsigned short y;
      unsigned short y0;
      unsigned short ph;
      f = *((unsigned short*)data); data += sizeof(unsigned short);
      y = *((unsigned short*)data); data += sizeof(unsigned short);
      y0 = *((unsigned short*)data); data += sizeof(unsigned short);
      ph = *((unsigned short*)data);
      tp->f = decodeReal(f,0.0f,PI);
      tp->y = decodeRealLog(y)*(real)M;
      tp->y0 = decodeRealLog(y0)*(real)M;
      tp->ph = decodeReal(ph,-PI,PI);
      
      bStart = (flags & 1)?true:false;
      tailStart = (flags & 2)?true:false;
      bEnd = (flags & 4)?true:false;
      tailEnd = (flags & 8)?true:false;
      tp->h = h1;
      tp->M = M;
      
      track *t = ta->getTrack(index);
      if(t && !bStart) {
	t->push_back_tpoint(tp);
	if(bEnd) {
	  t->end = assigntime[c];
	}
	if(tailEnd) {
	  t->tailEnd = true;
	}
      } else {
	track *t1;
	if(precursorIndex != USHRT_MAX) {
	  track *precursor = ta->getTrack(precursorIndex);
	  t1 = ta->create(precursor,this,res,index);
	  if(precursor) precursor->descendant = t1;
	} else {
	  t1 = ta->create(NULL,this,res,index);
	}
	if(tailStart)
	  t1->tailStart = true;
	t1->push_back_tpoint(tp);
	t1->m_p = tp->ph;
	t1->m_pDescendant = tp->ph;
	t1->currtime = assigntime[c];
	t1->start = assigntime[c];
	addTrack(c,t1);
      }
    }
    assigntime[c]++;
  }
}

void sms :: zeroPad(long n)
{
  sines2->grow(n);
  sines2->writePos += n;
}

int sms :: writeTrackPointsToFile(FILE *fp)
{
  int bytes = 0;
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  unsigned short h1 = hSynth.read(hSynth.readPos);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
  if(fp) {
    fwrite(&h1,sizeof(unsigned short),1,fp); bytes += sizeof(unsigned short);
    char c = channels;
    fwrite(&c,sizeof(char),1,fp); bytes += sizeof(char);
    for(int c=0;c<channels;c++) {
      unsigned short nTracks = 0;
      long start = ftell(fp);
      fwrite(&nTracks,sizeof(unsigned short),1,fp); bytes += sizeof(unsigned short);
#ifdef MULTITHREADED
      pthread_mutex_lock(&trackMutex[c]);
#endif
      for(list<track*>::iterator tt=trax[c]->begin(); 
	  tt != trax[c]->end(); 
	  ) {
	track *t = (*tt);      
	if(t->end < synthtime) {
	  if(!t->descendant||t->descendant->isDone()) {
	    list<track*>::iterator eraseMe = tt;
	    tt++;
	    trax[c]->erase(eraseMe);
	    ta->destroy(t);
	  } else {
	    tt++;
	  }
	} else if(synthtime >= t->start) {
	  track *t = (*tt);      
	  t->currtime = synthtime;
	  trackpoint *tp = t->getTrackPoint(synthtime);
	  if(tp) {
	    char flags = 0;
	    bool bStart;
	    bool bEnd;
	    bool tailStart;
	    bool tailEnd;
	    if(t->isStart(synthtime))
	      bStart = true;
	    else
	      bStart = false;
	    if(t->tailStart && t->isStart(synthtime))
	      tailStart = true;
	    else
	      tailStart = false;
	    if(t->isEnd(synthtime))
	      bEnd = true;
	    else
	      bEnd = false;
	    if(t->tailEnd && t->isEnd(synthtime))
	      tailEnd = true;
	    else
	      tailEnd = false;
	    
	    if(bStart)
	      flags += 1;
	    if(tailStart)
	      flags += 2;
	    if(bEnd)
	      flags += 4;
	    if(tailEnd)
	      flags += 8;
	    
	    fwrite(&flags,sizeof(char),1,fp); bytes += sizeof(char);
	    fwrite(&(t->index),sizeof(unsigned short),1,fp); bytes += sizeof(unsigned short);
	    if(t->precursor) {
	      fwrite(&(t->precursor->index),sizeof(unsigned short),1,fp); bytes += sizeof(unsigned short);
	    } else {
	      unsigned short none = USHRT_MAX;
	      fwrite(&none,sizeof(unsigned short),1,fp); bytes += sizeof(unsigned short);
	    }
	    unsigned short f = encodeReal(tp->f,0.0f,PI);
	    unsigned short y = encodeRealLog(tp->y/(real)M);
	    unsigned short y0 = encodeRealLog(tp->y0/(real)M);
	    unsigned short ph = encodeReal(tp->ph,-PI,PI);
	    fwrite(&f,sizeof(unsigned short),1,fp); bytes += sizeof(unsigned short);
	    fwrite(&y,sizeof(unsigned short),1,fp); bytes += sizeof(unsigned short);
	    fwrite(&y0,sizeof(unsigned short),1,fp); bytes += sizeof(unsigned short);
	    fwrite(&ph,sizeof(unsigned short),1,fp); bytes += sizeof(unsigned short);
	    nTracks++;
	  }
	  tt++;
	} else {
	  break;
	}
      }
#ifdef MULTITHREADED
      pthread_mutex_unlock(&trackMutex[c]);
#endif
      long end = ftell(fp);
      fseek(fp,start,SEEK_SET);
      fwrite(&nTracks,sizeof(unsigned short),1,fp);
      fseek(fp,end,SEEK_SET);    
    }
  }
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  hSynth.advance(1);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
  synthtime++;
  return bytes;
}

void sms :: setH(unsigned short h)
{
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  hSynth.write(h);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
}

long sms :: addTrackPoints(grain *g0, grain *g1, grain *g2)
{
  // set magnitude profile of signel constant frequency peak
  if(!bPeakSet) {
    for(int k=-Nover2;k<=Nover2;k++) {
      peak1[k+N] = norm2(g1->peak[(k+N)%N]);
    }
    bPeakSet = true;
  }

  setH(g1->h);
  // split channels
  c2evenodd(g0, x0[0], x0[1]);
  c2evenodd(g1, x1[0], x1[1]);
  c2evenodd(g2, x2[0], x2[1]);

  for(int c=0;c<channels;c++) {
    calcmags(mag0[c], x0[c],p);
    calcmags(mag1[c], x1[c],1);
    calcmags(mag2[c], x2[c],q);
    extractTrackPoints(x1[c],mag0[c],mag1[c],mag2[c],currtime,trackPointListBuffer[c],g1->h,c);
  }
  currtime++;
  return 1;
}

inline real sms :: merit(trackpoint *tp0, trackpoint *tp1, real m0, real m1, real dti2, real dMCoeff, real *df, real maxDF2)
{
  (*df) = m1*tp1->f - m0*tp0->f;
  real df2 = square(*df);
  if(df2 > maxDF2) return df2;
  if(tp0->y==0.0f || tp1->y==0.0f) return df2;
  real dM = dBApprox((m1*tp1->y)/(m0*tp0->y));
  return (df2+square(dMCoeff*dM));
}

bool sms :: nearestTrackPoint(tplist *tpl, trackpoint *tp0, real m0, real m1, real dti2, tplist::iterator *minpos, real *minMerit2, real maxMerit2, real maxDF2) 
{
  (*minMerit2) = 1e9;
  if(tpl == NULL) return false;
  bool bSet = false;

  for(tplist::iterator tpi=tpl->begin(); 
      tpi!=tpl->end();
      ++tpi) {

    trackpoint *tp1 = (*tpi);
    real df;
    real Merit2 = merit(tp0,tp1,m0,m1,dti2,dMCoeff,&df,maxDF2);
    if(m0!=m1) Merit2*=localFavorRatio;
    if(Merit2 < (*minMerit2) && Merit2 < maxMerit2 && square(df) < maxDF2) {
      (*minMerit2) = Merit2;
      (*minpos) = tpi;
      bSet = true;
    }
  }
  return bSet;
}

bool sms :: contTrack(track *t, trackpoint *tp, int c)
{
#ifdef MULTITHREADED
  pthread_mutex_lock(&trackMutex[c]);
#endif
  t->push_back(tp);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&trackMutex[c]);
#endif
  return true;
}      

bool sms :: adoptTrack(track *precursor, 
		       sms *lender,
		       trackpoint *tp,
		       real m,
		       real dt,
		       int c)
{
#ifdef MULTITHREADED
  pthread_mutex_lock(&lender->trackMutex[c]);
#endif
  trackpoint *last = precursor->back();
#ifdef MULTITHREADED
  pthread_mutex_unlock(&lender->trackMutex[c]);
#endif
  if(tp->M>last->M) {
    if(dt==1.0) {
      if(lender->res==2) {
        trackpoint *tp0 = new trackpoint();
        tp0->h = tp->h*2/lender->res;
        tp0->y = 0.5f*tp->y;
        tp0->y0 = tp0->y;
        tp0->f = 0.5f*tp->f;
        tp0->ph = tp->ph;
        tp0->time = last->time+1;
        tp0->M = last->M;
#ifdef MULTITHREADED
        pthread_mutex_lock(&lender->trackMutex[c]);
#endif
        precursor->push_back(tp0);
#ifdef MULTITHREADED
        pthread_mutex_unlock(&lender->trackMutex[c]);
#endif
        if(!tp->owner) 
          tp->bDelete = true;
      } else {
        trackpoint *tpend = new trackpoint();
        tpend->h = tp->h*2/lender->res;
        tpend->y = 0;
        tpend->y0 = 0;
        tpend->f = 0.5f*tp->f;
        tpend->ph = tp->ph;
        tpend->time = last->time+1;
        tpend->M = last->M;
        
        trackpoint *tp0 = new trackpoint();
        tp0->h = last->h*lender->res/2;
        tp0->y = 0;
        tp0->y0 = 0;
        tp0->f = last->f*2.0f;
        tp0->ph = last->ph;
        tp0->time = last->time/lender->res;
        tp0->M = M;
        track *t = ta->create(precursor,this,res);
#ifdef MULTITHREADED
        pthread_mutex_lock(&lender->trackMutex[c]);
#endif
        precursor->push_back(tpend);
        precursor->endTrack(false);
        precursor->descendant = t;
#ifdef MULTITHREADED
        pthread_mutex_unlock(&lender->trackMutex[c]);
#endif
        t->startTrack(tp0,false);
        t->push_back(tp);
        addTrack(c,t);
      }
    } else if(dt==2.0) {
      track *t = ta->create(precursor,this,res);
      trackpoint *tpend0 = new trackpoint();
      trackpoint *tpend1 = new trackpoint();
      trackpoint *tp0 = new trackpoint();
      tp0->h = (last->h*lender->res)/2;
      tp0->y = 0;
      tp0->y0 = 0;
      tp0->f = last->f*2.0f;
      tp0->ph = last->ph;
      tp0->M = M;
      tp0->time = last->time/lender->res;
      
      tpend0->h = last->h;
      tpend0->y = 0.5f*last->y;
      tpend0->y0 = tpend0->y;
      tpend0->f = 0.5f*(last->f+0.5f*tp->f);
      tpend0->ph = canon(last->ph + 0.5f*(last->f+tpend0->f)*last->h);
      tpend0->time = last->time+1;
      tpend0->M = last->M;
      
      tpend1->h = (tp->h*2)/lender->res;
      tpend1->y = 0;
      tpend1->y0 = 0;
      tpend1->f = 0.5f*tp->f;
      tpend1->ph = tp->ph;
      tpend1->time = last->time+2;
      tpend1->M = last->M;
      
      t->startTrack(tp0,false);
      t->push_back(tp);
#ifdef MULTITHREADED
      pthread_mutex_lock(&lender->trackMutex[c]);
#endif
      precursor->push_back(tpend0);
      precursor->push_back(tpend1);
      precursor->endTrack(false);
      precursor->descendant = t;
#ifdef MULTITHREADED
      pthread_mutex_unlock(&lender->trackMutex[c]);
#endif
      addTrack(c,t);
    } else {
      abort();
    }
  } else {
    track *t = ta->create(precursor,this,res);

    trackpoint *tp0 = new trackpoint();
    trackpoint *tpend = new trackpoint();
    
    tp0->h = (last->h*2)/res;
    tp0->y = 0;
    tp0->y0 = 0;
    tp0->f = 0.5f*last->f;
    tp0->ph = last->ph;
    tp0->time = last->time*res;
    tp0->M = M;
    
    tpend->h = (tp->h*res)/2;
    tpend->y = 0;
    tpend->y0 = 0;
    tpend->f = tp->f*2.0f;
    tpend->time = last->time+1;
    tpend->ph = tp->ph;
    tpend->M = last->M;
    
    t->startTrack(tp0,false);
    if(res==2) {
      trackpoint *tp1 = new trackpoint();
      tp1->h = (last->h*2)/res;
      tp1->y = 0.5f*tp->y0;
      tp1->y0 = tp1->y;
      tp1->f = 0.5f*(0.5f*last->f+tp->f);
      tp1->ph = canon(tp0->ph + 0.5f*(tp0->f + tp1->f)*last->h);
      tp1->time = last->time*res+1;
      tp1->M = M;
      t->push_back(tp1);
    }
    t->push_back(tp);
#ifdef MULTITHREADED
    pthread_mutex_lock(&lender->trackMutex[c]);
#endif
    precursor->push_back(tpend);
    precursor->endTrack(false);
    precursor->descendant = t;
#ifdef MULTITHREADED
    pthread_mutex_unlock(&lender->trackMutex[c]);
#endif
    addTrack(c,t);
  }
  return true;
}

void sms :: markDuplicatesLo(long offset, sms *lo, long offsetlo, int c)
{
  if(!lo) return;
#ifdef MULTITHREADED
  pthread_mutex_lock(&lo->tplbMutex[c]);
#endif
  tplist *trackPointsL1 = 
    lo->trackPointListBuffer[c]->read(lo->trackPointListBuffer[c]->readPos+offset/res+offsetlo);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&lo->tplbMutex[c]);
#endif
#ifdef MULTITHREADED
    pthread_mutex_lock(&tplbMutex[c]);
#endif
  tplist *trackPointsM1 = 
    trackPointListBuffer[c]->read(trackPointListBuffer[c]->readPos+offset);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&tplbMutex[c]);
#endif
  
  tplist trackPoints0;
  
  for(tplist::iterator tpi=trackPointsM1->begin(); 
      tpi!=trackPointsM1->end();
      tpi++) {
    
    if((*tpi)->f > maxFMatch) break;
    trackPoints0.push_back((*tpi));
  }
  
  bool bDone = false;
  bool bLastDitch = false;
  
  while(!bDone) {      
    int nToCont = 0;
    int nCont = 0;
    
    for(tplist::iterator tpi = trackPoints0.begin(); 
	tpi!=trackPoints0.end();
	tpi++) {
      real F;
      tplist::iterator minL0; bool minL0Set = nearestTrackPoint(trackPointsL1,*tpi,1.0,0.5,1.0,&minL0,&F,maxMerit2Match, maxDF2Match);
      if(minL0Set) {
	nToCont++;
	(*tpi)->cont = (*minL0);
      } else {
	(*tpi)->cont = NULL;
      }
    }
      
    if(trackPointsL1) {
      for(tplist::reverse_iterator tpi = trackPointsL1->rbegin(); 
	  tpi!=trackPointsL1->rend();
	  tpi++) {      
	real F;
	(*tpi)->cont = NULL;
	if((*tpi)->f < minFMatch) break;
	tplist::iterator minL1; bool minL1Set = nearestTrackPoint(&trackPoints0,*tpi,0.5,1.0,1.0,&minL1,&F,maxMerit2Match, maxDF2Match);
	if(minL1Set) (*tpi)->cont = *minL1;
      }
    }
    
    for(tplist::iterator tpi = trackPoints0.begin(); 
	tpi!=trackPoints0.end();
	) {
      trackpoint *tp0 = (*tpi);
      trackpoint *tp1 = tp0->cont;
      if(tp1 != NULL) {
	if(bLastDitch || tp1->cont == tp0) {
	  nCont++;
	  bool bAlreadyMarked = false;
	  if(offset%res == 0) {
	    if(tp1->dup[1] != NULL || tp0->dup[1] != NULL)
	      bAlreadyMarked = true;
	  } else {
	    if(tp1->dup[2-2*offsetlo] != NULL || tp0->dup[2*offsetlo] != NULL)
	      bAlreadyMarked = true;
	  }
	  if(!bAlreadyMarked) {
	    if(offset%res == 0) {
	      tp1->dup[1] = tp0;
	      tp0->dup[1] = tp1;
	    } else {
	      tp1->dup[2-2*offsetlo] = tp0;
	      tp0->dup[2*offsetlo] = tp1;
	    }
	  }
	  tplist::iterator eraseMe = tpi;
	  tpi++;
	  trackPoints0.erase(eraseMe);	    
	  continue;
	}
      }
      tpi++;
    }
    
    bDone = (nToCont-nCont == 0);
    bLastDitch = (!bDone && nCont==0);
  }
  marktime[c]++;
}

void sms :: markDuplicates(long offset, sms *hi, sms *lo, int c)
{
  if(offset%res==0) {
    markDuplicatesLo(offset,lo,offset%res,c);
  } else {
    markDuplicatesLo(offset,lo,0,c);
    markDuplicatesLo(offset,lo,1,c);
  }
}

void sms :: addTrack(int c, track *t)
{
#ifdef MULTITHREADED
  pthread_mutex_lock(&trackMutex[c]);
#endif
  list<track*>::reverse_iterator i=trax[c]->rbegin();
  while(i!=trax[c]->rend()) {
    track *t0 = *i;
    if(t->start >= t0->start) {
      break;
    }
    i++;
  }
  trax[c]->insert(i.base(),t);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&trackMutex[c]);
#endif
}

void sms :: deleteTrackPoint(trackpoint *tp)
{
  delete tp;
}

bool sms :: connectTrackPoints(trackpoint *tp0, trackpoint *tp1, sms *hi, sms *lo, real dtlo, int c)
{
  bool isConn;
  bool allowCont;
  if(tp0->M == tp1->M) {
    allowCont = contTrack(tp0->owner, tp1, c);
    isConn = allowCont;
  } else if(tp0->M < tp1->M) {
    isConn = true;
    allowCont = lo->adoptTrack(tp0->owner, this, tp1, 0.5, dtlo, c);
  } else {
    isConn = true;
    allowCont = hi->adoptTrack(tp0->owner, this, tp1, 2.0, 1.0, c);
  }
  tp0->bConnected = true;
  if(isConn) {
    tp1->bConnected = true;
    if(tp0->dupcont != NULL) {
      trackpoint *dup = tp0->dupcont;
      dup->bConnected = true;
      if(!dup->owner)
	dup->bDelete = true;
    }
    
    trackpoint *dup2 = tp0->dup[2];
    if(dup2 && dup2 != tp1 && !dup2->owner) {
      dup2->bDelete = true;
    }

    for(int d=0;d<3;d++) {
      if(tp1->dup[d] && !tp1->dup[d]->owner && (d<2 || tp1->dup[d]->M < tp1->M)) {
	tp1->dup[d]->bDelete = true;
      }
    }
  }
  return allowCont;
}

void sms :: advanceTrackPoints(int c)
{
#ifdef MULTITHREADED
    pthread_mutex_lock(&tplbMutex[c]);
#endif
    trackPointListBuffer[c]->advance(1);
#ifdef MULTITHREADED
    pthread_mutex_unlock(&tplbMutex[c]);
#endif
}

long sms :: assignTrackPoints(long offset, sms *hi, sms *lo, int c)
{  
  if(offset%res == 0)
    return assignTrackPoints_(offset,hi,lo,1.0,0,c);
  else
    return assignTrackPoints_(offset,hi,lo,2.0,1,c);
}

long sms :: assignTrackPoints_(long offset, sms *hi, sms *lo, real dtlo, long offsetlo, int c)
{
#ifdef MULTITHREADED
  if(hi) pthread_mutex_lock(&hi->tplbMutex[c]);
#endif
  tplist *trackPointsH1 = 
    hi?hi->trackPointListBuffer[c]->read(hi->trackPointListBuffer[c]->readPos+hi->res*offset):NULL;  
#ifdef MULTITHREADED 
  if(hi) pthread_mutex_unlock(&hi->tplbMutex[c]);
#endif

#ifdef MULTITHREADED
  if(lo) pthread_mutex_lock(&lo->tplbMutex[c]);
#endif
    tplist *trackPointsL1 = 
    lo?lo->trackPointListBuffer[c]->read(lo->trackPointListBuffer[c]->readPos+offset/res+offsetlo):NULL;
#ifdef MULTITHREADED
	if(lo) pthread_mutex_unlock(&lo->tplbMutex[c]);
#endif

#ifdef MULTITHREADED
  pthread_mutex_lock(&tplbMutex[c]);
#endif
  tplist *trackPointsM1 = 
    trackPointListBuffer[c]->read(trackPointListBuffer[c]->readPos+offset);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&tplbMutex[c]);	
#endif
  
  tplist trackPoints0;
  
#ifdef MULTITHREADED
  pthread_mutex_lock(&trackMutex[c]);
#endif
  for(list<track*>::iterator tt=trax[c]->begin(); 
      tt != trax[c]->end(); ) {
    track *t = (*tt);
    t->bEnd = true;
    if(t->isEnded() || t->back()->time+1 != assigntime[c]) { t->bEnd = false; tt++; continue; }
    trackPoints0.push_back(t->back());
    tt++;
  }
#ifdef MULTITHREADED
  pthread_mutex_unlock(&trackMutex[c]);
#endif

  for(tplist::iterator tpi=trackPointsM1->begin(); 
      tpi!=trackPointsM1->end();
      ) {
    trackpoint *tp = (*tpi);
    if(tp->bDelete) {
      tplist::iterator eraseMe = tpi;
      tpi++;
      trackPointsM1->erase(eraseMe);
      deleteTrackPoint(tp);
    } else {
      tpi++;
    }
  }
  
  if(trackPointsL1) {
    for(tplist::iterator tpi=trackPointsL1->begin(); 
	tpi!=trackPointsL1->end();
	) {
      trackpoint *tp = (*tpi);
      if(tp->bDelete) {
	tplist::iterator eraseMe = tpi;
	tpi++;
	trackPointsL1->erase(eraseMe);
	deleteTrackPoint(tp);
      } else {
	tpi++;
      }
    }
  }

  if(trackPointsH1) {
    for(tplist::iterator tpi=trackPointsH1->begin(); 
	tpi!=trackPointsH1->end();
	) {
      trackpoint *tp = (*tpi);
      if(tp->bDelete) {
	tplist::iterator eraseMe = tpi;
	tpi++;
	trackPointsH1->erase(eraseMe);
	deleteTrackPoint(tp);
      } else {
	tpi++;
	}
    }
  }

  bool bDone = false;
  bool bLastDitch = false;
  
  while(!bDone) {
    int nToCont = 0;
    int nCont = 0;
    
    for(tplist::iterator tpi=trackPointsM1->begin(); 
	tpi!=trackPointsM1->end();
	tpi++) {
      real F;	
      (*tpi)->bConnect = false;
      (*tpi)->bConnected = false;
      tplist::iterator minM1; bool minM1Set = nearestTrackPoint(&trackPoints0,*tpi,1.0,1.0,1.0,&minM1,&F,maxMerit2,maxDF2);
      if(minM1Set) (*tpi)->cont = *minM1;
      else (*tpi)->cont = NULL;
    }
    
    if(trackPointsL1) {
      for(tplist::reverse_iterator tpi = trackPointsL1->rbegin(); 
	  tpi!=trackPointsL1->rend();
	  tpi++) {      
	(*tpi)->cont = NULL;
	(*tpi)->bConnect = false;
	(*tpi)->bConnected = false;
	if((*tpi)->f < minFMatch) break;
	real F;
	tplist::iterator minL1; bool minL1Set = nearestTrackPoint(&trackPoints0,*tpi,0.5f,1.0f,square(1.0f/dtlo),&minL1,&F,maxMerit2,maxDF2);
	if(minL1Set) (*tpi)->cont = *minL1;
      }
    }
    
    if(trackPointsH1) {
      for(tplist::iterator tpi = trackPointsH1->begin(); 
	  tpi!=trackPointsH1->end();
	  tpi++) {      
	(*tpi)->cont = NULL;
	(*tpi)->bConnect = false;
	(*tpi)->bConnected = false;
	real F;
	if((*tpi)->f < maxFMatch) {
	  tplist::iterator minH1; bool minH1Set = nearestTrackPoint(&trackPoints0,*tpi,2.0f,1.0f,1.0f,&minH1,&F,maxMerit2,maxDF2);
	  if(minH1Set) (*tpi)->cont = *minH1;
	}
      }
    }
    
    for(tplist::iterator tpi = trackPoints0.begin(); 
	tpi!=trackPoints0.end();
	tpi++) {
      
      (*tpi)->dupcont = NULL;
      (*tpi)->bConnected = false;
      real FM0, FL0, FH0;
      tplist::iterator minM0; bool minM0Set = nearestTrackPoint(trackPointsM1,*tpi,1.0f,1.0f,1.0f,&minM0,&FM0,maxMerit2,maxDF2);
      tplist::iterator minL0; bool minL0Set = nearestTrackPoint(trackPointsL1,*tpi,1.0f,0.5f,square(1.0f/dtlo),&minL0,&FL0,maxMerit2,maxDF2);
      tplist::iterator minH0; bool minH0Set = nearestTrackPoint(trackPointsH1,*tpi,1.0f,2.0f,1.0f,&minH0,&FH0,maxMerit2,maxDF2);
      
      if(minM0Set &&
	 ((FM0<=FH0 && FM0<=FL0)
	  ||(minL0Set && FL0<=FH0 && FL0<=FM0 && (*minL0)->dup[1-offsetlo] == (*minM0))
	  ||(minH0Set && FH0<=FL0 && FH0<=FM0 && (*minH0)->dup[1] == (*minM0)))
	 ) {
	if(offsetlo == 0 && minL0Set && (*minL0)->dup[1] == (*minM0)) {	    
	  (*tpi)->dupcont = *minL0;
	} else if(minH0Set && (*minH0)->dup[1] == (*minM0)) {
	  (*tpi)->dupcont = *minH0;
	}
	(*tpi)->contF = FM0;
	(*tpi)->cont = *minM0;
	nToCont++;
      } else if(minL0Set && FL0<=FM0 && FL0<=FH0) {
	if(minM0Set && (*minL0)->dup[1-offsetlo] == (*minM0)) {
	  (*tpi)->dupcont = *minM0;
	}
	(*tpi)->contF = FL0;
	(*tpi)->cont = *minL0;
	nToCont++;
      } else if(minH0Set && FH0<=FM0 && FH0<=FL0) {
	if(minM0Set && (*minH0)->dup[1] == (*minM0)) {
	  (*tpi)->dupcont = *minM0;
	}
	(*tpi)->contF = FH0;
	(*tpi)->cont = *minH0;
	nToCont++;
      } else {
	(*tpi)->cont = NULL;
      }
    }

    // Nominal connections - may be conflicts to be resolved
    for(tplist::iterator tpi = trackPoints0.begin(); 
	tpi!=trackPoints0.end();
	tpi++
	) {
      trackpoint *tp0 = (*tpi);
      trackpoint *tp1 = tp0->cont;
      if(tp1 != NULL) {
	if(bLastDitch || (tp1->cont == tp0)) {
	  tp1->bConnect = true;
	}
      }
    }
    
    // Make connections and resolve conflicts between duplicates and between last ditch connections
    for(tplist::iterator tpi = trackPoints0.begin(); 
	tpi!=trackPoints0.end();
	) {
      
      trackpoint *tp0 = (*tpi);
      trackpoint *tp1 = tp0->cont;
      
      if(tp0->bDelete) { 
	tplist::iterator eraseMe = tpi;
	tpi++;
	trackPoints0.erase(eraseMe);
	continue;
      }
      if(tp1 != NULL && tp1->bConnect) {
	if(tp0->dupcont != NULL && tp0->dupcont->bConnect) {
	  if(!tp1->bConnected && !tp0->dupcont->bConnected) {
	    if(!tp0->bConnected && !tp1->bDelete && (tp0->dupcont->cont == NULL || tp0->contF <= tp0->dupcont->cont->contF)) {
	      nCont++;
	      if(connectTrackPoints(tp0,tp1,hi,lo,dtlo,c))
		tp0->owner->bEnd = false;
	      tp0->dupcont->bConnect = false;
	    } else if(tp0->dupcont->cont != NULL && !tp0->dupcont->cont->bConnected && !tp0->dupcont->bDelete && !tp0->dupcont->cont->bDelete) {
	      nCont++;
	      if(connectTrackPoints(tp0->dupcont->cont,tp0->dupcont,hi,lo,dtlo,c))
		tp0->dupcont->cont->owner->bEnd = false;
	      tp1->bConnect = false;
	    }
	  }
	} else if(!tp0->bConnected && !tp1->bConnected && !tp1->bDelete) {
	  nCont++;
	  if(connectTrackPoints(tp0,tp1,hi,lo,dtlo,c)) {
	    tp0->owner->bEnd = false;
	  }
	}
      }
      if(tp0->bConnected || tp0->bDelete) {
	tplist::iterator eraseMe = tpi;
	tpi++;
	trackPoints0.erase(eraseMe);
      } else {
	tpi++;
      }
    }
    
    for(tplist::iterator tpi=trackPointsM1->begin(); 
	tpi!=trackPointsM1->end();
	) {
      trackpoint *tp = (*tpi);
      if(tp->bConnected ||tp->bDelete) {
	tplist::iterator eraseMe = tpi;
	tpi++;
	trackPointsM1->erase(eraseMe);
      } else {
	tpi++;
      }
      if(tp->bDelete) {
	deleteTrackPoint(tp);
      }
    }
    
    if(trackPointsL1) {
      for(tplist::iterator tpi=trackPointsL1->begin(); 
	  tpi!=trackPointsL1->end();
	  ) {      
	trackpoint *tp = (*tpi);
	if(tp->bConnected||tp->bDelete) {
	  tplist::iterator eraseMe = tpi;
	  tpi++;
	  trackPointsL1->erase(eraseMe);
	} else {
	  tpi++;
	}
	if(tp->bDelete) {
	  deleteTrackPoint(tp);
	}
      }
    }
    
    if(trackPointsH1) {
      for(tplist::iterator tpi=trackPointsH1->begin(); 
	  tpi!=trackPointsH1->end();
	  ) {    
	trackpoint *tp = (*tpi);
	if(tp->bConnected || tp->bDelete) {
	  tplist::iterator eraseMe = tpi;
	  tpi++;
	  trackPointsH1->erase(eraseMe);
	} else {
	  tpi++;
	}
	if(tp->bDelete) {
	  deleteTrackPoint(tp);
	}
      }
    }
    
    bDone = (nToCont-nCont == 0);
    bLastDitch = (!bDone && nCont==0);
  }
  
#ifdef MULTITHREADED
  pthread_mutex_lock(&trackMutex[c]);
#endif
  for(list<track*>::iterator tt=trax[c]->begin(); 
      tt != trax[c]->end(); ) {
    track *t = (*tt);      
    if(t->bEnd) {
      t->endTrack(true);
    }
    if(t->size() < minNpts && !t->descendant) {
      list<track*>::iterator eraseMe = tt;
      tt++;
      trax[c]->erase(eraseMe);
      ta->destroy(t);  
    } else {
      tt++;
    }
  }

#ifdef MULTITHREADED
  pthread_mutex_unlock(&trackMutex[c]);
#endif
  
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  assigntime[c]++;
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif

  return 1;
}

long sms :: startNewTracks(long offset, int c)
{
#ifdef MULTITHREADED
  pthread_mutex_lock(&tplbMutex[c]);
#endif
  tplist *trackPoints = trackPointListBuffer[c]->read(trackPointListBuffer[c]->readPos+offset);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&tplbMutex[c]);
#endif

  for(tplist::iterator tpi=trackPoints->begin(); 
      tpi!=trackPoints->end();
      ) {
    trackpoint *tp = (*tpi);
    
    if(!tp->bDelete
       &&
       tp->y > startThresh*sqrtmagmax) {
      track *t = ta->create(NULL,this,res);
      t->startTrack(tp,true);
      addTrack(c,t);	
      for(int d=0;d<2;d++) {
	if(tp->dup[d])
	  if(!tp->dup[d]->owner)
	    tp->dup[d]->bDelete = true;
      }
    } else {
      deleteTrackPoint(tp);
    }                 
    tplist::iterator eraseMe = tpi;
    tpi++;
    trackPoints->erase(eraseMe);
  }
  return 1;
}

void sms :: synthTracks(real a, real f0, real f1)
{
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  unsigned short h1 = hSynth.read(hSynth.readPos);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
  real h2 = (a*(real)h1);			  
  real fScale0, fScale1;
  real mScale;
  h2 *= (real)M;
  mScale = 1.0f / (real)M;
  fScale0 = f0 * mScale;
  fScale1 = f1 * mScale;    
  h2cum += h2;
  int h2i = round2int(h2cum);
  h2cum -= h2i;
#ifdef MULTITHREADED
  pthread_mutex_lock(&bufferMutex);
#endif
  // extra slack for long fall times
  int grow = h2i<<1;
  sines2->N = grow;
  sines2->grow(grow);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&bufferMutex);
#endif

  for(int c=0;c<channels;c++) {
    list<track*> tracks;
#ifdef MULTITHREADED
    pthread_mutex_lock(&trackMutex[c]);
#endif
    for(list<track*>::iterator tt=trax[c]->begin();
        tt!=trax[c]->end(); ) {  
      track *t = (*tt);
      if(t->end <= synthtime) {
        if(!t->descendant||t->descendant->isDone()) {
          list<track*>::iterator eraseMe = tt;
          tt++;
          trax[c]->erase(eraseMe);
          ta->destroy(t);
        } else {
          tt++;
        }
      } else if(synthtime >= t->start) {
        t->synth(sines2,sines2->writePos,c,synthtime,h2i,fScale0,fScale1,mScale);
        tt++;
      } else {
        break;
      }
    }
#ifdef MULTITHREADED
    pthread_mutex_unlock(&trackMutex[c]);
#endif   
  }

#ifdef MULTITHREADED
  pthread_mutex_lock(&bufferMutex);
#endif
  sines2->writePos += h2i;
#ifdef MULTITHREADED
  pthread_mutex_unlock(&bufferMutex);
#endif
  samplePos += h2i;
#ifdef MULTITHREADED
  pthread_mutex_lock(&dataMutex);
#endif
  hSynth.advance(1);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&dataMutex);
#endif
  synthtime++;
}

long sms :: n_readable()
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

long sms :: read(audio *out, long n)
{
  long n_read;
#ifdef MULTITHREADED
  pthread_mutex_lock(&bufferMutex);
#endif
  n_read = outMixer->read(out, n); 
#ifdef MULTITHREADED
  pthread_mutex_unlock(&bufferMutex);
#endif
  return n_read;
}

void sms :: advance(long n) 
{
#ifdef MULTITHREADED
  pthread_mutex_lock(&bufferMutex);
#endif
  outMixer->advance(n);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&bufferMutex);
#endif
}

void sms :: c2evenodd(grain *eo, grain *even, grain *odd) {
  _c2evenodd(eo->x, even->x, odd->x, eo->N);
}

void sms :: calcmags(real *mag, grain *g, real q) {
  for(int k=0;k<=Nover2;k++) {
    real m = norm2(g->x[k])*q;
    mag[k] = m;
    if(magmax < m) magmax = m;
  }
  sqrtmagmax = sqrt(magmax);
}

peak *sms :: makePeak(real *mag, real *mag2, int k)
{
  real v0,v1,v2;
  v0 = mag2[k-1];
  v1 = mag2[k];
  v2 = mag2[k+1];
  real y1 = v1-v0;
  real y2 = v2-v0;
  real a = 0.5f*(y2 - 2*y1);
  real b = a==0?2.0f:1.0f-y1/a;
      
  peak *p = pa->create();
  p->x = k-1+0.5f*b;
  p->k = k;
  p->y2 = v0-0.25f*a*b*b;
  int k0 = round2int(p->x);
  real kf = k0<p->x?p->x-k0:k0-p->x;
  int k1 = k0<p->x?k0+1:k0-1;
  p->y = (1.0f-kf)*mag[k0] + kf*mag[k1];
  p->tp = NULL;
  p->tp2 = NULL;
  p->tn = NULL;
  p->tn2 = NULL;
  return p;
}

void sms :: adjustPeaks(list<peak*> &peaks,
			real *mag0,
			real *mag1,
			real *mag2,
			real *dec,
			int peakWidth
			) {

  for(int k=0;k<N;k++) {
    dec[k] = 0.0;
  }

  // determine troughs and remove peaks based on minPeakTopRatio
  // don't determine maginitude yet
  for(list<peak *>::iterator tpi = peaks.begin();
      tpi != peaks.end();
      ) {
    list<peak*>::iterator eraseMe = tpi;
    peak *p = (*tpi);

    peak *pp = NULL;
    if(tpi!=peaks.begin()) {
      pp = *(--tpi);
      tpi++;
    }

    peak *pn = NULL;
    tpi++;
    if(tpi != peaks.end()) {
      pn = (*tpi);
    }

    if(p->tp && pp && pp->tn
       &&
       p->x - p->tp->x > p->x - p->tp2->x
       &&
       pp->tn->x - pp->x > pp->tn2->x - pp->x) {
    } else {
      p->tp = p->tp2;
    }
    
    if(p->tn && pn && pn->tp
       &&
       p->tn->x - p->x > p->tn2->x - p->x
       &&
       pn->x - pn->tp->x > pn->x - pn->tp2->x) {
    } else {
      p->tn = p->tn2;
    }

    peak *tp = p->tp;
    peak *tn = p->tn;

    int k1 = p->k;
    int k0 = tp->k;
    real kf0;
    if(k0 < k1-peakWidth) {
      k0 = k1-peakWidth;
      kf0 = 0;
    } else {
      kf0 = k0 > tp->x ? k0 - tp->x : tp->x - k0;
    }
    int k2 = tn->k;
    real kf2;
    if(k2 > k1+peakWidth) {
      k2 = k1+peakWidth;
      kf2 = 0;
    } else {
      kf2 = k2 > tn->x ? k2 - tn->x : tn->x - k2;
    }

    real m = 0.0f;
    real mtop = 0.0f;

    real mbase = mag2[k0] + mag2[k2];
    if(k0 < tp->x) {
      real m0 = ((mag2[k0])+(mag2[k0+1]))*(1.0f-kf0);
      m += m0;
      mtop += max(0.0f,(m0 - mbase)*(1.0f-kf0));
    } else {
      real m0 = ((mag2[k0])+(mag2[k0+1]));
      m += m0;
      mtop += max(0.0f,m0 - mbase);
      m0 = ((mag2[k0-1])+(mag2[k0]))*kf0;
      m += m0;
      mtop += max(0.0f,(m0 - mbase)*kf0);
    }
    
    if(k2 < tn->x) {
      real m0 = ((mag2[k2-1])+(mag2[k2]));
      m += m0;
      mtop += max(0.0f,m0 - mbase);
      m0 = ((mag2[k2])+(mag2[k2+1]))*kf2;
      m += m0;
      mtop += max(0.0f,(m0 - mbase)*kf2);
    } else {
      real m0 = ((mag2[k2-1])+(mag2[k2]))*(1.0f-kf2);
      m += m0;
      mtop += max(0.0f,(m0 - mbase)*(1.0f-kf2));
    }
    
    for(int k=k0+1;k<k2-1;k++) {
      real m0 = ((mag2[k])+(mag2[k+1]));
      m += m0;
      mtop += max(0.0f,m0 - mbase);
    }

    if(mtop < minPeakTopRatio*m) {
      peaks.erase(eraseMe);
      if(pp && (!pn || pp->y > pn->y)) {
	pp->tn = p->tn;
	pp->tn2 = p->tn2;
      } else if(pn && (!pp || pn->y > pp->y)) {
	pn->tp = p->tp;
	pn->tp2 = p->tp2;
      }
    }
  }

  // determine magnitude
  for(list<peak *>::iterator tpi = peaks.begin();
      tpi != peaks.end();
      tpi++
      ) {
    peak *p = (*tpi);
    
    peak *tp = p->tp;
    peak *tn = p->tn;
       
    int k1 = p->k;
    int ko1 = k1 > p->x ? -1 : 1;
    real kf1 = k1 > p->x ? k1 - p->x : p->x - k1; 
    int k0 = tp->k;
    real kf0;
    if(k0 < k1-peakWidth) {
      k0 = k1-peakWidth;
      kf0 = 0;
    } else {
      kf0 = k0 > tp->x ? k0 - tp->x : tp->x - k0;
    }
    int k2 = tn->k;
    real kf2;
    if(k2 > k1+peakWidth) {
      k2 = k1+peakWidth;
      kf2 = 0;
    } else {
      kf2 = k2 > tn->x ? k2 - tn->x : tn->x - k2;
    }

    real m1 = 0.0f;

    if(k0 < tp->x) {
      m1 += ((mag1[k0])+(mag1[k0+1]))*(1.0f-kf0);
    } else {
      m1 += ((mag1[k0])+(mag1[k0+1]));
      m1 += ((mag1[k0-1])+(mag1[k0]))*kf0;
    }
    
    if(k2 < tn->x) {
      m1 += ((mag1[k2-1])+(mag1[k2]));
      m1 += ((mag1[k2])+(mag1[k2+1]))*kf2;
    } else {
      m1 += ((mag1[k2-1])+(mag1[k2]))*(1.0f-kf2);
    }
    
    for(int k=k0+1;k<k2-1;k++) {
      m1 += ((mag1[k])+(mag1[k+1]));
    }
    
    m1 *= 0.5f;

    for(int k=max(1,k1-peakWidth+1);k<=k0;k++) {
      real d = (1.0f-kf1)*peak1[k-k1+N] + kf1*peak1[k-k1+N+ko1];
      real m = d*(p->y);
      m1 += m;
      dec[k] += m;
    }
    for(int k=k2;k<min(k1+peakWidth,Nover2-1);k++) {
      real d = (1.0f-kf1)*peak1[k-k1+N] + kf1*peak1[k-k1+N+ko1];
      real m = d*(p->y);
      m1 += m;
      dec[k] += m;
    }
    p->m = m1;
  }

  for(list<peak *>::iterator tpi = peaks.begin();
      tpi != peaks.end();
      ) {
    peak *p = (*tpi);

    list<peak*>::iterator eraseMe = tpi;
    peak *pp = NULL;
    if(tpi!=peaks.begin()) {
      pp = *(--tpi);
      tpi++;
    }
    peak *pn = NULL;
    tpi++;
    if(tpi != peaks.end()) {
      pn = (*tpi);
    }

    peak *tp;
    peak *tn;
    tp = p->tp;
    tn = p->tn;

    int k1 = p->k;
    int k0 = tp->k;
    real kf0;
    if(k0 < k1-peakWidth) {
      k0 = k1-peakWidth;
      kf0 = 0;
    } else {
      kf0 = k0 > tp->x ? k0 - tp->x : tp->x - k0;
    }
    int k2 = tn->k;
    real kf2;
    if(k2 > k1+peakWidth) {
      k2 = k1+peakWidth;
      kf2 = 0;
    } else {
      kf2 = k2 > tn->x ? k2 - tn->x : tn->x - k2;
    }

    real m = p->m;
    real mdec = 0;

    if(k0 < tp->x) {
      mdec += (dec[k0]+dec[k0+1])*(1.0f-kf0);
    } else {
      mdec += (dec[k0]+dec[k0+1]);
      mdec += (dec[k0-1]+dec[k0])*kf0;
    }
    
    if(k2 < tn->x) {
      mdec += (dec[k2-1]+dec[k2]);
      mdec += (dec[k2]+dec[k2+1])*kf2;
    } else {
      mdec += (dec[k2-1]+dec[k2])*(1.0f-kf2);
    }
    
    for(int k=k0+1;k<k2-1;k++) {
      mdec += (dec[k]+dec[k+1]);
    }

    mdec *= 0.5;

    if(mdec > maxDecRatio*m) {
      peaks.erase(eraseMe);
      m -= mdec;
      if(pp && pn) {
	real d = square(pn->x - p->x) + square(pp->x - p->x);
	pp->m += m*square(pn->x - p->x)/d;
	pn->m += m*square(pp->x - p->x)/d;
      } else if(pp) {
	pp->m += m;
      } else if(pn) {
	pn->m += m;
      }
      pa->destroy(p);
      continue;
    } else {
      m -= mdec;
      p->m = m;
    }
  }
}

void sms :: extractTrackPoints(grain *g, 
			       real *mag0, real *mag1, real *mag2,
			       long currtime,
			       TrackPointListBuffer *trackPointListBuffer,
			       unsigned short h1,
			       int c)
{
  real thmin = square(peakThresh)*magmax;
  list<peak*> peaks;
  peak* trough1 = NULL;
  peak* trough2 = NULL;

  peak *t0 = pa->create();
  t0->k = 1;
  t0->x = (real)1;
  t0->y = mag1[0];
  t0->y2 = mag2[0];
  t0->tp = NULL;
  t0->tp2 = NULL;
  t0->tn = NULL;
  t0->tn2 = NULL;

  peak *t1 = pa->create();
  t1->k = Nover2-1;
  t1->x = (real)(Nover2-1);
  t1->y = mag1[Nover2-1];
  t1->y2 = mag2[Nover2-1];
  t1->tp = NULL;
  t1->tp2 = NULL;
  t1->tn = NULL;
  t1->tn2 = NULL;

  trough2 = t0;

  int k00=0;
  int k01=Nover2-1;
  int k10=0;
  int k11=Nover2-1;
  int kp = -1;

  for(int k=1;k<Nover2-1;k++) {
    if( (mag2[k] > thmin)
        &&
        (mag2[k] > mag2[k-1])
        &&
        (mag2[k] >= mag2[k+1])
        ) {

      kp = k;
      if(k>=kStart-peakWidth && k<=kEnd+peakWidth) {
        peak *p = makePeak(mag1,mag2,k);
        p->tn2 = t1;
        if(trough1 && trough1->tn == NULL) {
          p->tp = trough1;
          trough1->tn = p;
        }
        if(trough2 && trough2->tn2 == NULL) {
          p->tp2 = trough2;
          trough2->tn2 = p;
        }
        peaks.push_back(p);
      }
    } else if((mag2[k] <= mag2[k-1])
              &&
              (mag2[k] <= mag2[k+1])
              ) {

      peak *p = makePeak(mag1,mag2,k);
      trough2 = p;
      if(!peaks.empty()) { 
        peaks.back()->tn2 = p;
        p->tp2 = peaks.back();
      }
    }
    if((mag0[k] <= mag0[k-1])
       &&
       (mag0[k] <= mag0[k+1])
       ) {   
      if(peaks.empty())
        k00 = k;
      if(kp <= kEnd)
        k01 = k;
    }
    if((mag1[k] <= mag1[k-1])
       &&
       (mag1[k] <= mag1[k+1])
       ) {      
      if(peaks.empty())
        k10 = k;
      if(kp <= kEnd)
        k11 = k;
      peak *p = makePeak(mag1,mag1,k);
      trough1 = p;
      if(!peaks.empty() && peaks.back()->tn == NULL) {
        peaks.back()->tn = p;
        p->tp = peaks.back();
      }
    }
  }
  real m0=0.0f;
  real m1=0.0f;
  for(int k=k00;k<=k01;k++) {
    m0 += mag0[k];
    m1 += mag1[k];
  }
  real f = (m1==0.0f)?1.0f:min(1.0f,m0/m1);

  adjustPeaks(peaks,
	      mag0,
	      mag1,
	      mag2,
	      dec,
	      peakWidth);

  tplist *trackPoints = new tplist;
  for(list<peak*>::iterator pp=peaks.begin();
      pp!=peaks.end();
      pp++) {

    peak *p = (*pp);
    if(p->x>=kStart-0.5 && p->x<=kEnd+0.5 && p->m>0) {
      real y = sqrt(f*p->m*mNorm);
      trackpoint *tp = new trackpoint(g,p->x,y,N,M,h1,currtime);
      trackPoints->push_back(tp);
    }
  }

  pa->destroyAll();
#ifdef MULTITHREADED
  pthread_mutex_lock(&tplbMutex[c]);
#endif
  trackPointListBuffer->write(trackPoints);
#ifdef MULTITHREADED
  pthread_mutex_unlock(&tplbMutex[c]);
#endif
}

}
