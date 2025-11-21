/**********************************************************************

  Audacity: A Digital Audio Editor

  SBSMSBase.h

  ClaytonOtey

  This abstract class contains all of the common code for an
  effect that uses SBSMS to do its processing (TimeScale)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_SBSMS__
#define __AUDACITY_EFFECT_SBSMS__

#if USE_SBSMS

#include "StatefulEffect.h"
#include <sbsms.h>

using namespace _sbsms_;

class LabelTrack;
class TimeWarper;

class BUILTIN_EFFECTS_API SBSMSBase /* not final */ : public StatefulEffect
{
public:
    bool Process(EffectInstance& instance, EffectSettings& settings) override;
    void setParameters(double rateStart, double rateEnd, double pitchStart, double pitchEnd, SlideType rateSlideType,
                       SlideType pitchSlideType, bool bLinkRatePitch, bool bRateReferenceInput, bool bPitchReferenceInput);
    void setParameters(double tempoRatio, double pitchRatio); // Constant ratio (tempoRatio, pitchRatio)
    static double getInvertedStretchedTime(double rateStart, double rateEnd, SlideType slideType, double outputTime);
    static double getRate(double rateStart, double rateEnd, SlideType slideType, double t);

protected:
    TranslatableString mProxyEffectName { XO("SBSMS Time / Pitch Stretch") };
    // This supplies the abstract virtual function, but in fact this symbol
    // does not get used:  this class is either a temporary helper, or else
    // GetSymbol() is overridden further in derived classes.
    ComponentInterfaceSymbol GetSymbol() const override { return mProxyEffectName; }

private:
    EffectType GetType() const override;

    bool ProcessLabelTrack(LabelTrack* track);
    /*!
     @pre `orig.NChannels() == out.NChannels()`
     */
    void Finalize(
        WaveTrack& orig, const WaveTrack& out, const TimeWarper& warper);

    double rateStart, rateEnd, pitchStart, pitchEnd;
    bool bLinkRatePitch, bRateReferenceInput, bPitchReferenceInput;
    SlideType rateSlideType;
    SlideType pitchSlideType;
    int mCurTrackNum;
    float mTotalStretch;

    friend class ChangeTempoBase;
    friend class ChangePitchBase;
};

#endif

#endif
