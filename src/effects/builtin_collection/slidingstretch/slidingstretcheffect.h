/*
* Audacity: A Digital Audio Editor
*/
/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeScaleBase.h

  Clayton Otey

**********************************************************************/
#pragma once

#include "au3-builtin-effects/SBSMSBase.h"
#include "au3-command-parameters/ShuttleAutomation.h"
#include "au3-effects/Effect.h"

namespace au::effects {
struct SlidingStretchEffectParams
{
    double m_RatePercentChangeStart = 0.;
    double m_RatePercentChangeEnd = 0.;
    double m_PitchHalfStepsStart = 0.;
    double m_PitchHalfStepsEnd = 0.;
    double m_PitchPercentChangeStart = 0.;
    double m_PitchPercentChangeEnd = 0.;
};

class SlidingStretchEffect : public EffectWithSettings<SlidingStretchEffectParams, SBSMSBase>
{
public:
    static inline SlidingStretchEffectParams*
    FetchParameters(SlidingStretchEffect& e, EffectSettings&)
    {
        return &e.m_params;
    }

    static const ComponentInterfaceSymbol Symbol;

    SlidingStretchEffect();
    virtual ~SlidingStretchEffect() override;

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    ::EffectType GetType() const override;
    EffectGroup GetGroup() const override { return EffectGroup::PitchAndTempo; }

    // Effect implementation

    std::any BeginPreview(const EffectSettings& settings) override;
    bool Process(EffectInstance& instance, EffectSettings& settings) override;
    double CalcPreviewInputLength(
        const EffectSettings& settings, double previewLength) const override;

public:
    // SlidingStretchEffect implementation

    static double PercentChangeToRatio(double percentChange);
    static double HalfStepsToPercentChange(double halfSteps);
    static double PercentChangeToHalfSteps(double percentChange);

    bool bPreview;
    double previewSelectedDuration;
    SlideType slideTypeRate;
    SlideType slideTypePitch;
    SlidingStretchEffectParams m_params;

    const EffectParameterMethods& Parameters() const override;

    static constexpr EffectParameter RatePercentStart {
        &SlidingStretchEffectParams::m_RatePercentChangeStart,
        L"RatePercentChangeStart",
        0.0,
        -90.0,
        500,
        1
    };
    static constexpr EffectParameter RatePercentEnd {
        &SlidingStretchEffectParams::m_RatePercentChangeEnd,
        L"RatePercentChangeEnd",
        0.0,
        -90.0,
        500,
        1
    };
    static constexpr EffectParameter HalfStepsStart {
        &SlidingStretchEffectParams::m_PitchHalfStepsStart,
        L"PitchHalfStepsStart",
        0.0,
        -12.0,
        12.0,
        1,
        2 // Step is one but we leave the possibility of showing cent values.
    };
    static constexpr EffectParameter HalfStepsEnd {
        &SlidingStretchEffectParams::m_PitchHalfStepsEnd,
        L"PitchHalfStepsEnd",
        0.0,
        -12.0,
        12.0,
        1,
        2
    };
    static constexpr EffectParameter PitchPercentStart {
        &SlidingStretchEffectParams::m_PitchPercentChangeStart,
        L"PitchPercentChangeStart",
        0.0,
        -50.0,
        100.0,
        1
    };
    static constexpr EffectParameter PitchPercentEnd {
        &SlidingStretchEffectParams::m_PitchPercentChangeEnd,
        L"PitchPercentChangeEnd",
        0.0,
        -50.0,
        100.0,
        1
    };
};
}
