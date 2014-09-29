/**********************************************************************

  Audacity: A Digital Audio Editor

  SBSMSEffect.h

  ClaytonOtey

  This abstract class contains all of the common code for an
  effect that uses SBSMS to do its processing (TimeScale)

**********************************************************************/

#if USE_SBSMS

#ifndef __AUDACITY_EFFECT_SBSMS__
#define __AUDACITY_EFFECT_SBSMS__

#include "Effect.h"

#include "sbsms.h"
using namespace _sbsms_;

class EffectSBSMS : public Effect {
 public:
   virtual bool Process();
   void setParameters(double rateStart, double rateEnd, double pitchStart, double pitchEnd,
                      SlideType rateSlideType, SlideType pitchSlideType,
                      bool bLinkRatePitch, bool bRateReferenceInput, bool bPitchReferenceInput);

 private:
   bool ProcessLabelTrack(Track *track);
   double rateStart, rateEnd, pitchStart, pitchEnd;
   bool bLinkRatePitch, bRateReferenceInput, bPitchReferenceInput;
   SlideType rateSlideType;
   SlideType pitchSlideType;
   int mCurTrackNum;
   double mCurT0;
   double mCurT1;
   float mTotalStretch;
};

#endif

#endif
