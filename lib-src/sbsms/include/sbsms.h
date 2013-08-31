// -*- mode: c++ -*-
#ifndef SBSMS_INCLUDE
#define SBSMS_INCLUDE

#include <stdio.h>

#undef WXUNUSED
#define WXUNUSED( x ) 

namespace _sbsms_ {

typedef float t_fft[2];
typedef t_fft audio;
typedef long long int SampleCountType;
typedef long long int TimeType;
typedef unsigned char TrackIndexType;

enum {
  maxBands = 10,
  numQualityParams = 52
};

struct SBSMSQualityParams {
  int bands;
  int H;
  int N[maxBands];
  int N0[maxBands];
  int N1[maxBands];
  int N2[maxBands];
  int res[maxBands];
};

class SBSMSQuality {
 public:
  SBSMSQuality(const SBSMSQualityParams *params);
  SBSMSQualityParams params;
  long getFrameSize();
  long getMaxPresamples();
};

extern const SBSMSQualityParams SBSMSQualityStandard;

struct SBSMSFrame {
  float ratio0;
  float ratio1;
  audio *buf;
  long size;
};

typedef long (*SBSMSResampleCB)(void *cbData, SBSMSFrame *frame);

class SBSMSInterface {
 public:
  virtual ~SBSMSInterface() {}
  virtual long samples(audio * WXUNUSED(buf), long WXUNUSED(n)) { return 0; }
  virtual float getStretch(float t)=0;
  virtual float getPitch(float t)=0;
  virtual long getPresamples()=0;
  virtual SampleCountType getSamplesToInput()=0;
  virtual SampleCountType getSamplesToOutput()=0;
};

class SBSMSTrackPoint {
 public:
  virtual ~SBSMSTrackPoint() {}
  virtual float getF()=0;
  virtual float getM()=0;
  virtual float getPhase()=0;
};

class SBSMSTrack {
 public:
  virtual ~SBSMSTrack() {}
  virtual SBSMSTrackPoint *getSBSMSTrackPoint(const TimeType &time)=0;
  virtual TrackIndexType getIndex()=0;
  virtual bool isFirst(const TimeType &synthtime)=0;
  virtual bool isLast(const TimeType &synthtime)=0;
};

class SBSMSRenderer {
 public:
  virtual ~SBSMSRenderer() {}
  virtual void startFrame() {}
  virtual void startTime(int WXUNUSED(c), const TimeType & WXUNUSED(time), int WXUNUSED(n)) {}
  virtual void render(int WXUNUSED(c), SBSMSTrack * WXUNUSED(t)) {}
  virtual void endTime(int WXUNUSED(c)) {}
  virtual void endFrame() {}
  virtual void end(const SampleCountType & WXUNUSED(samples)) {}
};

enum SBSMSError {
  SBSMSErrorNone = 0,
  SBSMSErrorInvalidRate
};

class SBSMSImp;

class SBSMS {
 public:
  SBSMS(int channels, SBSMSQuality *quality, bool bSynthesize);
  ~SBSMS();

  long read(SBSMSInterface *iface, audio *buf, long n);
  void addRenderer(SBSMSRenderer *renderer);
  void removeRenderer(SBSMSRenderer *renderer);
  long renderFrame(SBSMSInterface *iface);
  long getInputFrameSize();
  SBSMSError getError();
  friend class SBSMSImp;
 protected:
  SBSMSImp *imp;
};

enum SlideType {
  SlideIdentity = 0,
  SlideConstant,
  SlideLinearInputRate,
  SlideLinearOutputRate,
  SlideLinearInputStretch,
  SlideLinearOutputStretch,
  SlideGeometricInput,
  SlideGeometricOutput
};

class SlideImp;

class Slide {
 public:
  Slide(SlideType slideType, float rate0 = 1.0f, float rate1 = 1.0f, const SampleCountType &n = 0);
  ~Slide();
  float getTotalStretch();
  float getStretchedTime(float t);
  float getRate(float t);
  float getStretch(float t);
  float getRate();
  float getStretch();
  void step();
 protected:
  SlideImp *imp;
};
 
class SBSMSInterfaceSlidingImp;

class SBSMSInterfaceSliding : public SBSMSInterface {
public:
  SBSMSInterfaceSliding(Slide *rateSlide, 
                        Slide *pitchSlide, 
                        bool bPitchReferenceInput, 
                        const SampleCountType &samplesToInput, 
                        long preSamples,
                        SBSMSQuality *quality);
  virtual ~SBSMSInterfaceSliding();
  virtual float getStretch(float t);
  virtual float getPitch(float t);
  virtual long getPresamples();
  virtual SampleCountType getSamplesToInput();
  virtual SampleCountType getSamplesToOutput();

  friend class SBSMSInterfaceSlidingImp;
protected:
  SBSMSInterfaceSlidingImp *imp;
};

class ResamplerImp;

class Resampler {
 public:
  Resampler(SBSMSResampleCB func, void *data, SlideType slideType = SlideConstant);
  ~Resampler();
  long read(audio *audioOut, long frames);
  void reset();
  long samplesInOutput();

 protected:
  ResamplerImp *imp;
};

}

#endif
