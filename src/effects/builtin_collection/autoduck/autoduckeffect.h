/*
 * Audacity: A Digital Audio Editor
 */
/**********************************************************************

  Audacity: A Digital Audio Editor

  autoduckeffect.h

  Markus Meyer

**********************************************************************/
#pragma once

#include "au3-command-parameters/ShuttleAutomation.h"
#include "au3-effects/StatefulEffect.h"

#include <cfloat>

class WaveChannel;
class WaveTrack;

namespace au::effects {
class AutoDuckEffect : public StatefulEffect
{
public:
    static inline AutoDuckEffect* FetchParameters(AutoDuckEffect& e, EffectSettings&)
    {
        return &e;
    }

    static const ComponentInterfaceSymbol Symbol;

    AutoDuckEffect();
    virtual ~AutoDuckEffect();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    ::EffectType GetType() const override;
    ::EffectGroup GetGroup() const override { return EffectGroup::VolumeAndCompression; }

    // Effect implementation

    bool Init() override;
    bool Process(::EffectInstance& instance, EffectSettings& settings) override;

    double mDuckAmountDb = DuckAmountDb.def;
    double mInnerFadeDownLen = InnerFadeDownLen.def;
    double mInnerFadeUpLen = InnerFadeUpLen.def;
    double mOuterFadeDownLen = OuterFadeDownLen.def;
    double mOuterFadeUpLen = OuterFadeUpLen.def;
    double mThresholdDb = ThresholdDb.def;
    double mMaximumPause = MaximumPause.def;

private:
    // AutoDuckEffect implementation

    bool ApplyDuckFade(int trackNum, WaveChannel& track, double t0, double t1);

    const WaveTrack* mControlTrack {};

    // The effectexecutionscenario discards everything on the track outside the selection and sets `mT0` to 0 before processing a preview.
    // Also, auto duck has a control track, and it must undergo the same time shift for the correct processing of a preview.
    // On the other hand, ::Init() is called by the execution scenario when
    // 1. opening an effect
    // 2. if preview is used, after the preview's processing has been rendered onto a temporary track for playback.
    //
    // In summary:
    // 1. open effect
    //   a. mT0 = 1.5s (for example)
    //   b. Init()
    // 2. preview
    //   a. mT0 = 0
    //   b. Process
    //   c. mT0 = 1.5
    //   d. Init()
    // By 2.b., we must somehow know that `mT0` was originally 1.5.
    //
    // Workaround: use `Init()` to do this, and store it in a separate variable.
    // (We really need this effect context rework ...)
    double mSelectionT0 = 0.0;

protected:
    const EffectParameterMethods& Parameters() const override;

public:
    static constexpr EffectParameter DuckAmountDb {
        &AutoDuckEffect::mDuckAmountDb, L"DuckAmountDb", -12.0, -24.0, 0.0, 1
    };
    static constexpr EffectParameter InnerFadeDownLen {
        &AutoDuckEffect::mInnerFadeDownLen, L"InnerFadeDownLen", 0.0, 0.0, 3.0, 1
    };
    static constexpr EffectParameter InnerFadeUpLen {
        &AutoDuckEffect::mInnerFadeUpLen, L"InnerFadeUpLen", 0.0, 0.0, 3.0, 1
    };
    static constexpr EffectParameter OuterFadeDownLen {
        &AutoDuckEffect::mOuterFadeDownLen, L"OuterFadeDownLen", 0.5, 0.0, 3.0, 1
    };
    static constexpr EffectParameter OuterFadeUpLen {
        &AutoDuckEffect::mOuterFadeUpLen, L"OuterFadeUpLen", 0.5, 0.0, 3.0, 1
    };
    static constexpr EffectParameter ThresholdDb {
        &AutoDuckEffect::mThresholdDb, L"ThresholdDb", -30.0, -100.0, 0.0, 1
    };
    static constexpr EffectParameter MaximumPause {
        &AutoDuckEffect::mMaximumPause, L"MaximumPause", 1.0, 0.0, DBL_MAX, 1
    };
};
}
