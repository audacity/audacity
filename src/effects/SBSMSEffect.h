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

class WaveTrack;

class EffectSBSMS : public Effect {
 public:
   static bool bInit;
   virtual bool Process();
   void setParameters(double rateStart, double rateEnd, double pitchStart, double pitchEnd, bool bPreAnalyze);

 private:
   bool ProcessLabelTrack(Track *track);
   double rateStart, rateEnd, pitchStart, pitchEnd;
   bool bPreAnalyze;
   int mCurTrackNum;
   double mCurT0;
   double mCurT1;
   real mTotalStretch;
};

#endif

#endif
