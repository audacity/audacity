/*
* Audacity: A Digital Audio Editor
*/
/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeScaleBase.cpp

  Clayton Otey

*******************************************************************//**

\class SlidingStretchEffect
\brief A SlidingStretchEffect does high quality sliding time scaling/pitch shifting

*//*******************************************************************/

#if USE_SBSMS

#include "slidingstretcheffect.h"
#include "au3-utility/MemoryX.h"
#include <cmath>

namespace au::effects {
const EffectParameterMethods& SlidingStretchEffect::Parameters() const
{
    static CapturedParameters<
        SlidingStretchEffect, RatePercentStart, RatePercentEnd, HalfStepsStart,
        HalfStepsEnd, PitchPercentStart, PitchPercentEnd>
    parameters;
    return parameters;
}

//
// SlidingStretchEffect
//

const ComponentInterfaceSymbol SlidingStretchEffect::Symbol { wxT("Sliding stretch"),
                                                              XO("Sliding stretch") };

SlidingStretchEffect::SlidingStretchEffect()
{
    slideTypeRate = SlideLinearOutputRate;
    slideTypePitch = SlideLinearOutputRate;
    bPreview = false;
    previewSelectedDuration = 0.0;

    SetLinearEffectFlag(true);
}

SlidingStretchEffect::~SlidingStretchEffect()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol SlidingStretchEffect::GetSymbol() const
{
    return Symbol;
}

TranslatableString SlidingStretchEffect::GetDescription() const
{
    return XO("Allows continuous changes to the tempo and/or pitch");
}

ManualPageID SlidingStretchEffect::ManualPage() const
{
    return L"Sliding_Stretch";
}

// EffectDefinitionInterface implementation

::EffectType SlidingStretchEffect::GetType() const
{
    return EffectTypeProcess;
}

// Effect implementation

double SlidingStretchEffect::CalcPreviewInputLength(
    const EffectSettings& settings, double previewLength) const
{
    double inputLength = settings.extra.GetDuration();
    if (inputLength == 0.0) {
        return 0.0;
    } else {
        const auto& params = GetSettings(settings);
        double rateStart1 = PercentChangeToRatio(params.m_RatePercentChangeStart);
        double rateEnd1 = PercentChangeToRatio(params.m_RatePercentChangeEnd);
        double tOut = previewLength / inputLength;
        double t = SBSMSBase::getInvertedStretchedTime(
            rateStart1, rateEnd1, slideTypeRate, tOut);
        return t * inputLength;
    }
}

std::any SlidingStretchEffect::BeginPreview(const EffectSettings& settings)
{
    previewSelectedDuration = settings.extra.GetDuration();
    return { CopyableValueRestorer { bPreview, true } };
}

bool SlidingStretchEffect::Process(EffectInstance& instance, EffectSettings& settings)
{
    const auto& params = GetSettings(settings);
    double pitchStart1 = PercentChangeToRatio(params.m_PitchPercentChangeStart);
    double pitchEnd1 = PercentChangeToRatio(params.m_PitchPercentChangeEnd);
    double rateStart1 = PercentChangeToRatio(params.m_RatePercentChangeStart);
    double rateEnd1 = PercentChangeToRatio(params.m_RatePercentChangeEnd);

    if (bPreview) {
        double t = (mT1 - mT0) / previewSelectedDuration;
        rateEnd1 = SBSMSBase::getRate(rateStart1, rateEnd1, slideTypeRate, t);
        pitchEnd1 = SBSMSBase::getRate(pitchStart1, pitchEnd1, slideTypePitch, t);
    }

    SBSMSBase::setParameters(
        rateStart1, rateEnd1, pitchStart1, pitchEnd1, slideTypeRate,
        slideTypePitch, false, false, false);
    return SBSMSBase::Process(instance, settings);
}

double SlidingStretchEffect::PercentChangeToRatio(double percentChange)
{
    return 1.0 + percentChange / 100.0;
}

double SlidingStretchEffect::HalfStepsToPercentChange(double halfSteps)
{
    return 100.0 * (pow(2.0, halfSteps / 12.0) - 1.0);
}

double SlidingStretchEffect::PercentChangeToHalfSteps(double percentChange)
{
    return 12.0 * log2(PercentChangeToRatio(percentChange));
}
}

#endif // USE_SBSMS
