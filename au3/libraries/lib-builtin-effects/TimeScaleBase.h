/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeScaleBase.h

  Clayton Otey

**********************************************************************/
#pragma once

#if USE_SBSMS

#include "SBSMSBase.h"
#include "ShuttleAutomation.h"

class BUILTIN_EFFECTS_API TimeScaleBase : public SBSMSBase
{
public:
    static inline TimeScaleBase*
    FetchParameters(TimeScaleBase& e, EffectSettings&)
    {
        return &e;
    }

    static const ComponentInterfaceSymbol Symbol;

    TimeScaleBase();
    virtual ~TimeScaleBase();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;

    // Effect implementation

    std::any BeginPreview(const EffectSettings& settings) override;
    bool Process(EffectInstance& instance, EffectSettings& settings) override;
    double CalcPreviewInputLength(
        const EffectSettings& settings, double previewLength) const override;

protected:
    // TimeScaleBase implementation

    static double PercentChangeToRatio(double percentChange);
    static double HalfStepsToPercentChange(double halfSteps);
    static double PercentChangeToHalfSteps(double percentChange);

    bool bPreview;
    double previewSelectedDuration;
    SlideType slideTypeRate;
    SlideType slideTypePitch;
    double m_RatePercentChangeStart;
    double m_RatePercentChangeEnd;
    double m_PitchHalfStepsStart;
    double m_PitchHalfStepsEnd;
    double m_PitchPercentChangeStart;
    double m_PitchPercentChangeEnd;

    const EffectParameterMethods& Parameters() const override;

    static constexpr EffectParameter RatePercentStart {
        &TimeScaleBase::m_RatePercentChangeStart,
        L"RatePercentChangeStart",
        0.0,
        -90.0,
        500,
        1
    };
    static constexpr EffectParameter RatePercentEnd {
        &TimeScaleBase::m_RatePercentChangeEnd,
        L"RatePercentChangeEnd",
        0.0,
        -90.0,
        500,
        1
    };
    static constexpr EffectParameter HalfStepsStart {
        &TimeScaleBase::m_PitchHalfStepsStart,
        L"PitchHalfStepsStart",
        0.0,
        -12.0,
        12.0,
        1
    };
    static constexpr EffectParameter HalfStepsEnd {
        &TimeScaleBase::m_PitchHalfStepsEnd,
        L"PitchHalfStepsEnd",
        0.0,
        -12.0,
        12.0,
        1
    };
    static constexpr EffectParameter PitchPercentStart {
        &TimeScaleBase::m_PitchPercentChangeStart,
        L"PitchPercentChangeStart",
        0.0,
        -50.0,
        100.0,
        1
    };
    static constexpr EffectParameter PitchPercentEnd {
        &TimeScaleBase::m_PitchPercentChangeEnd,
        L"PitchPercentChangeEnd",
        0.0,
        -50.0,
        100.0,
        1
    };
};

#endif // USE_SBSMS
