#include "sbsms.h"
#include "real.h"
#include <math.h>
#include <algorithm>
using namespace std;

namespace _sbsms_ {

class SlideImp {
public:
  virtual ~SlideImp() {}
  virtual float getTotalStretch()=0;
  virtual float getStretchedTime(float t)=0;
  virtual float getInverseStretchedTime(float t)=0;
  virtual float getRate(float t)=0;
  virtual float getStretch(float t)=0;
  virtual float getMeanStretch(float t0, float t1)=0;
  virtual float getRate()=0;
  virtual float getStretch()=0;
  virtual void step()=0;
};

class IdentitySlide : public SlideImp {
public:
  IdentitySlide() {
  }
  float getTotalStretch() {
    return 1.0f;
  }
  float getStretchedTime(float t) {
    return t;
  }
  float getInverseStretchedTime(float t) {
    return t;
  }
  float getRate(float t) {
    return 1.0f;
  }
  float getStretch(float t) {
    return 1.0f;
  }
  float getMeanStretch(float t0, float t1) {
    return 1.0f;
  }
  float getRate() {
    return 1.0f;
  }
  float getStretch() {
    return 1.0f;
  }
  void step() {
  }
};

class ConstantSlide : public SlideImp {
public:
  ConstantSlide(float rate) {
    this->rate = rate;
  }
  float getTotalStretch() {
    return 1.0f / rate;
  }
  float getStretchedTime(float t) {
    return t / rate;
  }
  float getInverseStretchedTime(float t) {
    return rate * t;
  }
  float getRate(float t) {
    return rate;
  }
  float getStretch(float t) {
    return 1.0f / rate;
  }
  float getMeanStretch(float t0, float t1) {
    return 1.0f / rate;
  }
  float getRate() {
    return rate;
  }
  float getStretch() {
    return 1.0f / rate;
  }
  void step() {
  }
protected:
  float rate;
};

class LinearInputRateSlide : public SlideImp {
public:
  LinearInputRateSlide(float rate0, float rate1, const SampleCountType &n) {
    this->rate0 = rate0;
    this->rate1 = rate1;
    if(n) {
      val = rate0;
      inc = (rate1 - rate0) / (double)n;
    }
  }
  float getTotalStretch() {
    return log(rate1 / rate0) / (rate1 - rate0);
  }
  float getStretchedTime(float t) {
    float ratet = getRate(t);
    return log(ratet / rate0) / (rate1 - rate0);
  }
  float getInverseStretchedTime(float t) {
    return (exp((rate1-rate0)*t)-1.0f)*rate0/(rate1-rate0);
  }
  float getRate(float t) {
    return rate0 + (rate1 - rate0) * t;
  }
  float getStretch(float t) {
    return 1.0f / getRate(t);
  }
  float getMeanStretch(float t0, float t1) {
    return log(getRate(t1)/getRate(t0))/((rate1-rate0)*(t1-t0));
  }
  float getRate() {
    return (float)val;
  }
  float getStretch() {
    return (float)(1.0 / val);
  }
  void step() {
    val += inc;
  }
protected:
  float rate0, rate1;
  double val, inc;
};

class LinearOutputRateSlide : public SlideImp {
public:
  LinearOutputRateSlide(float rate0, float rate1, const SampleCountType &n) {
    this->rate0 = rate0;
    this->rate1 = rate1;
    if(n) {
      val = 0.0;
      inc = 1.0 / (double)n;
    }
  }
  float getTotalStretch() {
    return 2.0f / (rate0 + rate1);
  }
  float getStretchedTime(float t) {
    return (sqrt(rate0 * rate0 + (rate1 * rate1 - rate0 * rate0) * t) - rate0) * 2.0f / (rate1 * rate1 - rate0 * rate0);
  }
  float getInverseStretchedTime(float t) {
    float r12 = rate1 * rate1 - rate0 * rate0;
    float s = 0.5f * r12 * t + rate0;
    return (s * s - rate0 * rate0) / r12;
  }
  float getRate(float t) {
    return sqrt(rate0 * rate0 + (rate1 * rate1 - rate0 * rate0) * t);
  }
  float getStretch(float t) {
    return 1.0f / getRate(t);
  }
  float getMeanStretch(float t0, float t1) {
    float r02 = rate0 * rate0;
    float r102 = rate1 * rate1 - r02;
    return 2.0f * (sqrt(r02 + r102*t1) - sqrt(r02 + r102 * t0)) / (r102 * (t1 - t0));
  }
  float getRate() {
    return getRate((float)val);
  }
  float getStretch() {
    return getStretch((float)val);
  }
  void step() {
    val += inc;
  }
protected:
  float rate0, rate1;
  double val, inc;
};

class LinearInputStretchSlide : public SlideImp {
public:
  LinearInputStretchSlide(float rate0, float rate1, const SampleCountType &n) {
    this->rate0 = rate0;
    this->rate1 = rate1;
    if(n) {
      val = 1.0 / rate0;
      inc = (1.0 / rate1 - 1.0 / rate0) / (double)n;
    }
  }
  float getTotalStretch() {
    return 0.5f / rate0 + 0.5f / rate1;
  }
  float getStretchedTime(float t) {
    return t / rate0 * (1.0f + 0.5f * t * (rate0 / rate1 - 1.0f));
  }
  float getInverseStretchedTime(float t) {
    float s = 1.0f / rate1 - 1.0f / rate0;
    return (-1.0f / rate0 + sqrt(1/(rate0*rate0)-2.0f*t*s)) / s;
  }
  float getRate(float t) {
    return 1.0f / getStretch(t);
  }
  float getStretch(float t) {
    return (1.0f / rate0 + (1.0f / rate1 - 1.0f / rate0) * t);
  }
  float getMeanStretch(float t0, float t1) {
    return ((t1 + t0) * (rate0 - rate1)  - 2.0f * rate1) / (2.0f * rate0 * rate1);
  }
  float getRate() {
    return (float)(1.0 / val);
  }
  float getStretch() {
    return (float)val;
  }
  void step() {
    val += inc;
  }
protected:
  float rate0, rate1;
  double val, inc;
};

class LinearOutputStretchSlide : public SlideImp {
public:
  LinearOutputStretchSlide(float rate0, float rate1, const SampleCountType &n) {
    this->rate0 = rate0;
    this->rate1 = rate1;
    c0 = rate0 / rate1;
    c1 = 1.0f / (rate0 * log(c0));
    if(n) {
      val = 0.0;
      inc = 1.0 / (double)n;
    }
  }
  float getTotalStretch() {
    return c1 * (c0 - 1.0f);
  }
  float getStretchedTime(float t) {
    return c1 * (pow(c0, t) - 1.0f);
  }
  float getInverseStretchedTime(float t) {
    return log(t/c1 + 1.0f) * c1 * rate0;
  }
  float getRate(float t) {
    return rate0 * pow(c0, -t);
  }
  float getStretch(float t) {
    return pow(c0, t) / rate0;
  }
  float getMeanStretch(float t0, float t1) {
    return (pow(c0,t1) - pow(c0,t0)) / (c1 * (t1 - t0));
  }
  float getRate() {
    return getRate((float)val);
  }
  float getStretch() {
    return getStretch((float)val);
  }
  void step() {
    val += inc;
  }
protected:
  float rate0, rate1;
  double val, inc;
  float c0, c1;
};

class GeometricInputSlide : public SlideImp {
public:
  GeometricInputSlide(float rate0, float rate1, const SampleCountType &n) {
    this->rate0 = rate0;
    this->rate1 = rate1;
    this->c0 = rate0 / rate1;
    this->log01 = log(c0);
    if(n) {
      val = rate0;
      inc = pow((double)rate1 / rate0, 1.0 / (double)n);
    }
  }
  float getTotalStretch() {
    return (rate0 - rate1) / (log01 * (rate0 * rate1));
  }
  float getStretchedTime(float t) {
    return (float)(pow(rate0 / rate1, t) - 1.0) / (rate0 * log01);
  }
  float getInverseStretchedTime(float t) {
    return log(t * rate0 * log01 + 1.0f) / log01;
  }
  float getRate(float t) {
    return rate0 * pow(rate1 / rate0, t);
  }
  float getStretch(float t) {
    return 1.0f / getRate(t);
  }
  float getMeanStretch(float t0, float t1) {
    return (pow(c0,t1) - pow(c0,t0)) / (rate0 * log01 * (t1 - t0));
  }
  float getRate() {
    return (float)val;
  }
  float getStretch() {
    return (float)(1.0 / val);
  }
  void step() {
    val *= inc;
  }
protected:
  float rate0, rate1, c0, log01;
  double val, inc;
};

class GeometricOutputSlide : public SlideImp {
public:
  GeometricOutputSlide(float rate0, float rate1, const SampleCountType &n) {
    this->rate0 = rate0;
    this->rate1 = rate1;
    log10 = log(rate1 / rate0);
    r10 = rate1 - rate0;
    totalStretch = getTotalStretch();
    if(n) {
      val = 0.0;
      inc = 1.0 / (double)n;
    }
  }
  float getTotalStretch() {
    return log(rate1 / rate0) / (rate1 - rate0);
  }
  float getStretchedTime(float t) {
    return log(r10 / rate0 * t + 1.0f) / r10;
  }
  float getInverseStretchedTime(float t) {
    return rate0 / r10 * (exp(t * r10) - 1.0f);
  }
  float getRate(float t) {
    float t1 = getStretchedTime(t) / totalStretch;
    return rate0 * pow(rate1 / rate0, t1);
  }
  float getStretch(float t) {
    return 1.0f / getRate(t);
  }
  float getMeanStretch(float t0, float t1) {
    return log((rate0 + r10 * t1)/(rate0 + r10 * t0)) /(r10 * (t1 - t0));
  }
  float getRate() {
    return getRate((float)val);
  }
  float getStretch() {
    return getStretch((float)val);
  }
  void step() {
    val += inc;
  }
protected:
  float rate0, rate1;
  float log10, r10, totalStretch;
  double val, inc;
};


Slide :: Slide(SlideType slideType, float rate0, float rate1, const SampleCountType &n) 
{
  if(slideType == SlideIdentity) {
    imp = new IdentitySlide();
  } else if(slideType == SlideConstant || rate0 == rate1) {
    imp = new ConstantSlide(rate0);
  } else if(slideType == SlideLinearInputRate) {
    imp = new LinearInputRateSlide(rate0,rate1,n);
  } else if(slideType == SlideLinearOutputRate) {
    imp = new LinearOutputRateSlide(rate0,rate1,n);
  } else if(slideType == SlideLinearInputStretch) {
    imp = new LinearInputStretchSlide(rate0,rate1,n);
  } else if(slideType == SlideLinearOutputStretch) {
    imp = new LinearOutputStretchSlide(rate0,rate1,n);
  } else if(slideType == SlideGeometricInput) {
    imp = new GeometricInputSlide(rate0,rate1,n);
  } else if(slideType == SlideGeometricOutput) {
    imp = new GeometricOutputSlide(rate0,rate1,n);
  }
}

Slide :: ~Slide()
{
  delete imp;
}

float Slide :: getTotalStretch()
{
  return imp->getTotalStretch();
}

float Slide :: getRate(float t)
{
  if(t > 1.0f) t = 1.0f;
  return imp->getRate(t);
}

float Slide :: getStretch(float t)
{
  if(t > 1.0f) t = 1.0f;
  return imp->getStretch(t);
}

float Slide :: getStretchedTime(float t)
{
  if(t > 1.0f) t = 1.0f;
  return imp->getStretchedTime(t);
}
  
float Slide :: getMeanStretch(float t0, float t1) {
  if(t0 < 0.0f) t0 = 0.0f;
  return imp->getMeanStretch(t0,t1);
}
  
float Slide :: getInverseStretchedTime(float t)
{
  return imp->getInverseStretchedTime(t);
}

float Slide :: getRate()
{
  return imp->getRate();
}

float Slide :: getStretch()
{
  return imp->getStretch();
}

void Slide :: step()
{
  imp->step();
}

class SBSMSInterfaceSlidingImp  {
public:
  friend class SBSMSInterfaceSliding;
  SBSMSInterfaceSlidingImp(Slide *rateSlide, 
                           Slide *pitchSlide, 
                           bool bPitchReferenceInput, 
                           const SampleCountType &samplesToInput,
                           long preSamples,
                           SBSMSQuality *quality);
  ~SBSMSInterfaceSlidingImp() {}
  inline float getStretch(float t);
  inline float getMeanStretch(float t0, float t1);
  inline float getPitch(float t);
  inline long getPresamples();
  SampleCountType getSamplesToInput();
  SampleCountType getSamplesToOutput();
protected:
  Slide *stretchSlide;
  Slide *pitchSlide;
  bool bPitchReferenceInput;
  float totalStretch;
  float stretchScale;
  long preSamples;
  SampleCountType samplesToInput;
  SampleCountType samplesToOutput;
};
  
SBSMSInterfaceSlidingImp :: SBSMSInterfaceSlidingImp(Slide *stretchSlide,
                                                     Slide *pitchSlide, 
                                                     bool bPitchReferenceInput,
                                                     const SampleCountType &samplesToInput,
                                                     long preSamples,
                                                     SBSMSQuality *quality)
{
  this->stretchSlide = stretchSlide;
  this->pitchSlide = pitchSlide;
  this->bPitchReferenceInput = bPitchReferenceInput;
  this->samplesToInput = samplesToInput;
  this->preSamples = preSamples;
  this->totalStretch = stretchSlide->getTotalStretch();
  this->samplesToOutput = (SampleCountType)((float)samplesToInput * totalStretch);
  stretchScale = 1.0f;

  if(quality) {
    SampleCountType samplesIn = 0;
    SampleCountType samplesOut = 0;
    float outFrameSizefloat = 0.0f;
    float stretch = 1.0f;
    int inFrameSize = quality->getFrameSize();
    while(samplesIn < samplesToInput) {
      float t = (float)samplesIn / (float)samplesToInput;
      float t1 =(float)(samplesIn + inFrameSize) / (float)samplesToInput;
      stretch = stretchSlide->getMeanStretch(t,t1);
      outFrameSizefloat += stretch * inFrameSize;
      int outFrameSize = (int) outFrameSizefloat;
      outFrameSizefloat -= (float) outFrameSize;
      samplesIn += inFrameSize;
      samplesOut += outFrameSize;
    }    
    SampleCountType samplesOutputed = samplesOut - lrintf(stretch * (samplesIn - samplesToInput));
    stretchScale = (float)samplesToOutput / (float)samplesOutputed;
  }
}

float SBSMSInterfaceSliding :: getStretch(float t) { return imp->getStretch(t); }
float SBSMSInterfaceSlidingImp :: getStretch(float t)
{
  return stretchScale * stretchSlide->getStretch(t);
}

float SBSMSInterfaceSliding :: getMeanStretch(float t0, float t1) { return imp->getMeanStretch(t0, t1); }
float SBSMSInterfaceSlidingImp :: getMeanStretch(float t0, float t1)
{
  return stretchSlide->getMeanStretch(t0, t1);
}
  
float SBSMSInterfaceSliding :: getPitch(float t) { return imp->getPitch(t); }
float SBSMSInterfaceSlidingImp :: getPitch(float t)
{
  if(bPitchReferenceInput) return pitchSlide->getRate(t);
  else return pitchSlide->getRate(min(1.0f,stretchSlide->getStretchedTime(t) / totalStretch));
}

long SBSMSInterfaceSliding :: getPresamples() { return imp->getPresamples(); }
long SBSMSInterfaceSlidingImp :: getPresamples()
{
  return preSamples;
}
  
SampleCountType SBSMSInterfaceSliding :: getSamplesToInput() { return imp->getSamplesToInput(); }
SampleCountType SBSMSInterfaceSlidingImp :: getSamplesToInput()
{
  return samplesToInput;
}

SampleCountType SBSMSInterfaceSliding :: getSamplesToOutput() { return imp->getSamplesToOutput(); }
SampleCountType SBSMSInterfaceSlidingImp :: getSamplesToOutput()
{
  return samplesToOutput;
}

SBSMSInterfaceSliding :: SBSMSInterfaceSliding(Slide *stretchSlide,
                                               Slide *pitchSlide, 
                                               bool bPitchReferenceInput,
                                               const SampleCountType &samplesToInput,
                                               long preSamples,
                                               SBSMSQuality *quality)
{
  imp = new SBSMSInterfaceSlidingImp(stretchSlide, pitchSlide, bPitchReferenceInput,
                                     samplesToInput, preSamples, quality);
}
   
SBSMSInterfaceSliding :: ~SBSMSInterfaceSliding()
{
  delete imp;
}

}
