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

// Soundtouch defines these as well, so get rid of them before including 
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#undef PACKAGE_BUGREPORT

#include "sbsms.h"

class WaveTrack;

class EffectSBSMS : public Effect {
 public:
   static bool bInit;
   virtual bool Process();
   void setParameters(double rateStart, double rateEnd, double pitchStart, double pitchEnd, int quality, bool bPreAnalyze);

 private:
   bool ProcessLabelTrack(Track *track);
   double rateStart, rateEnd, pitchStart, pitchEnd;
   int quality;
   bool bPreAnalyze;
   int mCurTrackNum;
   double mCurT0;
   double mCurT1;
   real mTotalStretch;
};

#endif

#endif
