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

class LabelTrack;

class EffectSBSMS /* not final */ : public Effect
{
public:
   bool Process() override;
   void setParameters(double rateStart, double rateEnd, double pitchStart, double pitchEnd,
                      SlideType rateSlideType, SlideType pitchSlideType,
                      bool bLinkRatePitch, bool bRateReferenceInput, bool bPitchReferenceInput);
   void setParameters(double tempoRatio, double pitchRatio);  // Constant ratio (tempoRatio, pitchRatio)
   static double getInvertedStretchedTime(double rateStart, double rateEnd, SlideType slideType, double outputTime);
   static double getRate(double rateStart, double rateEnd, SlideType slideType, double t);

protected:
   wxString mProxyEffectName { XO("SBSMS Time / Pitch Stretch") };
   wxString GetName() override { return mProxyEffectName; };

private:
   bool ProcessLabelTrack(LabelTrack *track);
   double rateStart, rateEnd, pitchStart, pitchEnd;
   bool bLinkRatePitch, bRateReferenceInput, bPitchReferenceInput;
   SlideType rateSlideType;
   SlideType pitchSlideType;
   int mCurTrackNum;
   double mCurT0;
   double mCurT1;
   float mTotalStretch;

   friend class EffectChangeTempo;
   friend class EffectChangePitch;
};

#endif

#endif
