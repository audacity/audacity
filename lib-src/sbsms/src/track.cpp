#include "track.h"
#include <math.h>
#include "utils.h"
#include "real.h"
#include "sbsms.h"
#include "sms.h"
#include <vector>
using namespace std;

namespace _sbsms_ {

#define SBSMS_TRACK_BLOCK 2048

TrackAllocator :: TrackAllocator(bool bManageIndex)
{
  this-> bManageIndex = bManageIndex;
}

TrackAllocator :: TrackAllocator(bool bManageIndex, unsigned short maxtrackindex)
{
  this-> bManageIndex = bManageIndex;
  gTrack.resize(maxtrackindex);
  init();
}

TrackAllocator :: ~TrackAllocator()
{
}

void TrackAllocator :: init()
{
  for(unsigned short k=0;k<gTrack.size();k++)
    gTrack[k] = NULL;
  while(!gTrackIndex.empty())
    gTrackIndex.pop();
#ifdef MULTITHREADED
	pthread_mutex_init(&taMutex,NULL);
#endif
}

track *TrackAllocator :: getTrack(unsigned short index)
{
  return gTrack[index];
}

int TrackAllocator :: size()
{
  return gTrack.size();
}

track *TrackAllocator :: create(track *precursor, sms *owner, int res, unsigned short index) 
{
  track *t = new track(precursor,owner,res);
  t->index = index;
  gTrack[index] = t;
  return t;
}

track *TrackAllocator :: create(track *precursor, sms *owner, int res) 
{
#ifdef MULTITHREADED
	pthread_mutex_lock(&taMutex);
#endif
  if(gTrackIndex.empty()) {
    unsigned short n = gTrack.size();
    gTrack.resize(n+SBSMS_TRACK_BLOCK);
    for(int k=n+SBSMS_TRACK_BLOCK-1;k>=n;k--) {
      gTrackIndex.push(k);
      gTrack[k] = NULL;
    }
  }
  unsigned short index = gTrackIndex.top();
  gTrackIndex.pop();
#ifdef MULTITHREADED
	pthread_mutex_unlock(&taMutex);
#endif
  return create(precursor, owner, res, index);
}

void TrackAllocator :: destroy(track *t)
{
#ifdef MULTITHREADED
	pthread_mutex_lock(&taMutex);
#endif
  gTrack[t->index] = NULL;
  if(bManageIndex) 
    gTrackIndex.push(t->index);
  delete t;
#ifdef MULTITHREADED
	pthread_mutex_unlock(&taMutex);
#endif
}

track :: track(track *precursor, sms *owner, int res) 
{
  this->owner = owner;
  this->res = res;
  this->descendant = NULL;
  this->precursor = precursor;
  this->end = LONG_MAX;
  this->rise = 0.3f;
  this->fall = 0.5f;
  tailEnd = 0;
  tailStart = 0;
  m_p = 0.0;
  currtime = 0;
}


void track :: endTrack(bool bTail)
{
  end = back()->time;
  if(bTail) {
    this->fall = min(1.5f,.25f + back()->y / point[point.size()-2]->y);
    tailEnd = 1;
    end++;
    trackpoint *f = new trackpoint(back());
    f->time = back()->time + 1;
    point.push_back(f);
  }
}

void track :: synth(SampleBuf *out,
                    long writePos,
                    int c,
                    long synthtime,
                    int steps,
                    real fScale0,
                    real fScale1,
                    real mScale) {
  if(point.size()==0) return;
  long k = synthtime - start;
  if(k>=(long)point.size()-1) return;
  currtime = synthtime;
  if(k<0) return;
  int k1 = k + 1;  

  tpoint *tp0 = point[k];
  tpoint *tp1 = point[k1];

  real w0 = tp0->f;
  real w1 = tp1->f;
  real ph0 = tp0->ph;
  real ph1 = tp1->ph;
  real h = tp0->h;
 
  real dp = ph1 - ph0;
  if(dp>PI) dp-=TWOPI;
  else if(dp<-PI) dp+=TWOPI;
  real dp0 = 0.5f*h*(w0 + w1);
  real dw = canon(dp - dp0)/h;

  if(k==0) {
    if(precursor) {
      m_p = precursor->m_pDescendant;
    }  else {    
      dw = 0;
    }
  }
  
  real dt = (real)steps;
  w0 = (w0+dw);
  w1 = (w1+dw);
  w0 = w0*fScale0;
  w1 = w1*fScale1;
  dp = dt*0.5f*(w0 + w1);
  real b = (w1 - w0)/(2.0f*dt);

  bool bEnd = (k1==(long)point.size()-1);
  bool bStart = (k==0);

  if(bStart && tailStart) {
    if(w0 < PI && w0 > -PI) {
      real ph = m_p;
      int rise = round2int(this->rise * (real)steps);
      real dm = mScale*tp1->y/(real)rise;
      real m = mScale*tp1->y - dm;
      for(int i=steps-1;i>=steps-rise+1;i--) {
        ph -= w0;     
        if(ph<-PI) ph += TWOPI;
        else if(ph>PI) ph -= TWOPI;
        out->buf[writePos+i][c] += m * COS(ph);
        m -= dm;
      }
    }
  } else if(bEnd && tailEnd) {
    if(w0 < PI && w0 > -PI) {
      real ph = m_p;
      int fall = round2int(this->fall * (real)steps);
      real dm = mScale*tp0->y/(real)fall;
      real m = mScale*tp0->y;
      for(int i=0;i<fall;i++) {
        out->buf[writePos+i][c] += m * COS(ph); 
        ph += w0;
        if(ph<-PI) ph += TWOPI;
        else if(ph>PI) ph -= TWOPI;
        m -= dm;
      }
    }
  } else  {
    real m = mScale*tp0->y;
    real dm = mScale*(tp1->y0 - tp0->y)/dt;
    real ph = m_p;
    real b2tt1 = b;
    real b2 = 2.0f*b;
    real dph;

    audio *o = &(out->buf[writePos]);
    for(int i=0;i<steps;i++) {
      dph = w0 + b2tt1;      
      if(dph < PI && dph > -PI) (*o)[c] += m * COS(ph);
      ph += dph;
      if(ph<-PI) ph += TWOPI;
      else if(ph>PI) ph -= TWOPI;
      b2tt1 += b2;
      m += dm;
      o++;
    }
  }

  if(bEnd) {
    if(descendant && descendant->back()->M < tp0->M) {
      m_pDescendant = canon(m_p + dp/dt*(real)(descendant->owner->samplePos - owner->samplePos));
    }
  } else if(bStart && tailStart) {
  } else {
    m_p = canon(m_p + dp);
    if(descendant && descendant->back()->M > tp0->M && (k1+res==(long)point.size()-1)) {
      m_pDescendant = canon(m_p + dp/dt*(real)(descendant->owner->samplePos - (owner->samplePos+steps)));
    }
  }
}

bool track :: isEnded()
{
  return (end!=LONG_MAX);
}

void track :: push_back_tpoint(tpoint *p)
{
  point.push_back(p);
}

void track :: push_back(trackpoint *p)
{
  point.push_back(p);
  p->owner = this;
}
  
void track :: startTrack(trackpoint *p, bool bTail)
{
  push_back(p);
  start = p->time;
  m_pDescendant = p->ph;
  if(bTail) {
    tailStart = 1;
    start--;
    trackpoint *s = new trackpoint(p);
    s->time = p->time - 1;
    point.insert(point.begin(),s);
    m_p = p->ph;
  }
  currtime = p->time-1;
}

trackpoint *track :: back() 
{
  return (trackpoint*)point.back(); 
}

trackpoint *track :: getTrackPoint(long time)
{
  long k = time-start;
  if(k<0 || k >= (long)point.size())
    return NULL;
  else
    return (trackpoint*)point[k];
}

bool track :: isStart(long synthtime)
{
  return (synthtime == start);
}

bool track :: isEnd(long synthtime)
{
  return (synthtime == end);
}

bool track :: isDone() 
{
  return (currtime-start+1 >= (long)point.size()-1);
}

long track :: size()
{
  return 1+end-start-tailEnd-tailStart;
}

track :: ~track() {
  if(precursor) precursor->descendant = NULL;
  if(descendant) descendant->precursor = NULL;
  for(vector<tpoint*>::iterator i = point.begin();
      i != point.end();
      i++) {
    delete (*i);
  }
}

}
