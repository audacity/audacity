/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeScaleBase.h

  Clayton Otey

**********************************************************************/
#pragma once

#include "SBSMSBase.h"
#include "au3-command-parameters/ShuttleAutomation.h"
#include "au3-effects/Effect.h"

struct TimeScaleBaseParams
{
    double m_RatePercentChangeStart = 0.;
    double m_RatePercentChangeEnd = 0.;
    double m_PitchHalfStepsStart = 0.;
    double m_PitchHalfStepsEnd = 0.;
    double m_PitchPercentChangeStart = 0.;
    double m_PitchPercentChangeEnd = 0.;
};

class BUILTIN_EFFECTS_API TimeScaleBase : public EffectWithSettings<TimeScaleBaseParams, SBSMSBase>
{
public:
    static inline TimeScaleBaseParams*
    FetchParameters(TimeScaleBase& e, EffectSettings&)
    {
        return &e.m_params;
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
    EffectGroup GetGroup() const override { return EffectGroup::PitchAndTempo; }

    // Effect implementation

    std::any BeginPreview(const EffectSettings& settings) override;
    bool Process(EffectInstance& instance, EffectSettings& settings) override;
    double CalcPreviewInputLength(
        const EffectSettings& settings, double previewLength) const override;

public:
    // TimeScaleBase implementation

    static double PercentChangeToRatio(double percentChange);
    static double HalfStepsToPercentChange(double halfSteps);
    static double PercentChangeToHalfSteps(double percentChange);

    bool bPreview;
    double previewSelectedDuration;
    SlideType slideTypeRate;
    SlideType slideTypePitch;
    TimeScaleBaseParams m_params;

    const EffectParameterMethods& Parameters() const override;

    static constexpr EffectParameter RatePercentStart {
        &TimeScaleBaseParams::m_RatePercentChangeStart,
        L"RatePercentChangeStart",
        0.0,
        -90.0,
        500,
        1
    };
    static constexpr EffectParameter RatePercentEnd {
        &TimeScaleBaseParams::m_RatePercentChangeEnd,
        L"RatePercentChangeEnd",
        0.0,
        -90.0,
        500,
        1
    };
    static constexpr EffectParameter HalfStepsStart {
        &TimeScaleBaseParams::m_PitchHalfStepsStart,
        L"PitchHalfStepsStart",
        0.0,
        -12.0,
        12.0,
        1
    };
    static constexpr EffectParameter HalfStepsEnd {
        &TimeScaleBaseParams::m_PitchHalfStepsEnd,
        L"PitchHalfStepsEnd",
        0.0,
        -12.0,
        12.0,
        1
    };
    static constexpr EffectParameter PitchPercentStart {
        &TimeScaleBaseParams::m_PitchPercentChangeStart,
        L"PitchPercentChangeStart",
        0.0,
        -50.0,
        100.0,
        1
    };
    static constexpr EffectParameter PitchPercentEnd {
        &TimeScaleBaseParams::m_PitchPercentChangeEnd,
        L"PitchPercentChangeEnd",
        0.0,
        -50.0,
        100.0,
        1
    };
};
