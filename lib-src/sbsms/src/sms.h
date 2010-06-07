#ifndef SMS_H
#define SMS_H

#include "track.h"
#include "grain.h"
#include "buffer.h"
#include "peak.h"
#include "sbsms.h"
#include "config.h"
#ifdef MULTITHREADED
#include "pthread.h"
#endif

#include <list>
using namespace std;

namespace _sbsms_ {

class renderer;
class sms : public SampleBufBase {
  friend class renderer;
 public:
  sms(int N, unsigned short M, unsigned short M_MAX, int res, int latency, real p, real q, real pad, int Nlo, int channels, TrackAllocator *ta, PeakAllocator *pa);
  void reset();
  void readTrackPointsFromFile(FILE *fp);
  int writeTrackPointsToFile(FILE *fp);
  void setH(unsigned short h);
  long addTrackPoints(grain *g0, grain *g1, grain *g2);
  long assignTrackPoints(long offset, sms *hi, sms *lo, int c);
  long startNewTracks(long offset, int c);
  void markDuplicates(long offset, sms *hi, sms *lo, int c);
  bool connectTrackPoints(trackpoint *tp0, trackpoint *tp1, sms *hi, sms *lo, real dtlo, int c);
  bool adoptTrack(track *precursor, sms *lender, trackpoint *tp, real m, real dt, int c);
  void synthTracks(real a, real f0, real f1);
  void advanceTrackPoints(int c);
  long nTrackPoints();
  long read(audio *out, long n);
  long n_readable();
  void advance(long n);
  void zeroPad(long n);
  void addTrack(int c, track *t);
  ~sms();


  bool bPhaseLock;
  int res;
  int N;
  unsigned short M;
  real m;
  int Nover2;
  RingBuffer<unsigned short> hAssign[2];
  RingBuffer<unsigned short> hSynth;
  real h2cum;
  long samplePos;
  TrackPointListBuffer* trackPointListBuffer[2];

  
#ifdef MULTITHREADED
  pthread_mutex_t dataMutex;
  pthread_mutex_t tplbMutex[2];
  pthread_mutex_t bufferMutex;
  pthread_mutex_t trackMutex[2];
#endif

 protected:
  unsigned short encodeReal(real x, real min, real max);
  float decodeReal(unsigned short x, real min, real max);
  unsigned short encodeRealLog(real x);
  float decodeRealLog(unsigned short x);
  bool terminal;
  void markDuplicatesLo(long offset, sms *lo, long offsetlo, int c);
  long assignTrackPoints_(long offset, sms *hi, sms *lo, real dtlo, long offsetlo, int c);
  void deleteTrackPoint(trackpoint *tp);
  void adjustPeaks(list<peak*> &peaks,
		   real *mag0,
		   real *mag1,
		   real *mag2,
		   real *dec,
		   int peakWidth);
  void phaseLock(trackpoint *tp0, trackpoint *tp1, long time);
  long assignTrackPoints(long offset, sms *hi, sms *lo, real dtlo, long offsetlo, int c);
  void stereoPhaseLock();
  bool contTrack(track *t, trackpoint *tp, int c);
  void extractTrackPoints(grain *g, real *mag0, real *mag1, real *mag2, long currtime, TrackPointListBuffer *tplb, unsigned short h1, int c);
  bool nearestTrackPoint(tplist *tpl, trackpoint *tp0, real m0, real m1, real dt, tplist::iterator *minpos, real *minFx, real maxMerit2, real maxDF2);
  int prune(trackpoint *tp);
  real merit(trackpoint *tp0, trackpoint *tp1, real m0, real m1, real dt, real dMCoeff, real *df, real maxDF2);
  void calcmags(real *mag, grain *g, real q);
  peak *makePeak(real *mag, real *mag2, int k);

  TrackAllocator *ta;
  PeakAllocator *pa;
  char *pData;
  int datasize;
  int channels;
  real p,q;
  int peakWidth;
  int maxPrunes;
  real peakThresh;
  real startThresh;
  real maxMerit2;
  real maxDF2;
  real dMCoeff;
  real maxMerit2PhaseLock;
  real maxDF2PhaseLock;
  real dMCoeffPhaseLock;
  real maxMerit2Match;
  real maxDF2Match;
  real dMCoeffMatch;
  int minNpts;
  real maxdBIncr;
  real maxdBIncrStitch;
  real maxFMatch;
  real minFMatch;
  int kStart;
  int kEnd;
  real mNorm;
  real minPeakTopRatio;
  real localFavorRatio;
  real maxDecRatio;
  
  list<track*> **trax;
  long currtime;
  long assigntime[2];
  long marktime[2];
  long synthtime;

  SampleBuf *sines2, *outMixer;
  grain* x0[2];
  grain* x1[2];
  grain* x2[2];
  
  void c2evenodd(grain *g, grain *, grain *);

  real* dec;
  real* dec2;
  real* mag0[2];
  real* mag1[2];
  real* mag2[2];
  real *peak1;
  bool bPeakSet;
  real sqrtmagmax;
  real magmax;

};

}

#endif
