/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeScaleBase.cpp

  Clayton Otey

*******************************************************************//**

\class TimeScaleBase
\brief An TimeScaleBase does high quality sliding time scaling/pitch shifting

*//*******************************************************************/

#if USE_SBSMS

#include "TimeScaleBase.h"
#include "MemoryX.h"
#include <cmath>

const EffectParameterMethods& TimeScaleBase::Parameters() const
{
    static CapturedParameters<
        TimeScaleBase, RatePercentStart, RatePercentEnd, HalfStepsStart,
        HalfStepsEnd, PitchPercentStart, PitchPercentEnd>
    parameters;
    return parameters;
}

//
// TimeScaleBase
//

const ComponentInterfaceSymbol TimeScaleBase::Symbol { wxT("Sliding Stretch"),
                                                       XO("Sliding Stretch") };

TimeScaleBase::TimeScaleBase()
{
    Parameters().Reset(*this);

    slideTypeRate = SlideLinearOutputRate;
    slideTypePitch = SlideLinearOutputRate;
    bPreview = false;
    previewSelectedDuration = 0.0;

    SetLinearEffectFlag(true);
}

TimeScaleBase::~TimeScaleBase()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol TimeScaleBase::GetSymbol() const
{
    return Symbol;
}

TranslatableString TimeScaleBase::GetDescription() const
{
    return XO("Allows continuous changes to the tempo and/or pitch");
}

ManualPageID TimeScaleBase::ManualPage() const
{
    return L"Sliding_Stretch";
}

// EffectDefinitionInterface implementation

EffectType TimeScaleBase::GetType() const
{
    return EffectTypeProcess;
}

// Effect implementation

double TimeScaleBase::CalcPreviewInputLength(
    const EffectSettings& settings, double previewLength) const
{
    double inputLength = settings.extra.GetDuration();
    if (inputLength == 0.0) {
        return 0.0;
    } else {
        double rateStart1 = PercentChangeToRatio(m_RatePercentChangeStart);
        double rateEnd1 = PercentChangeToRatio(m_RatePercentChangeEnd);
        double tOut = previewLength / inputLength;
        double t = SBSMSBase::getInvertedStretchedTime(
            rateStart1, rateEnd1, slideTypeRate, tOut);
        return t * inputLength;
    }
}

std::any TimeScaleBase::BeginPreview(const EffectSettings& settings)
{
    previewSelectedDuration = settings.extra.GetDuration();
    return { CopyableValueRestorer { bPreview, true } };
}

bool TimeScaleBase::Process(EffectInstance& instance, EffectSettings& settings)
{
    double pitchStart1 = PercentChangeToRatio(m_PitchPercentChangeStart);
    double pitchEnd1 = PercentChangeToRatio(m_PitchPercentChangeEnd);
    double rateStart1 = PercentChangeToRatio(m_RatePercentChangeStart);
    double rateEnd1 = PercentChangeToRatio(m_RatePercentChangeEnd);

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

double TimeScaleBase::PercentChangeToRatio(double percentChange)
{
    return 1.0 + percentChange / 100.0;
}

double TimeScaleBase::HalfStepsToPercentChange(double halfSteps)
{
    return 100.0 * (pow(2.0, halfSteps / 12.0) - 1.0);
}

double TimeScaleBase::PercentChangeToHalfSteps(double percentChange)
{
    return 12.0 * log2(PercentChangeToRatio(percentChange));
}

#endif // USE_SBSMS
