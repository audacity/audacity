/**********************************************************************

  Audacity: A Digital Audio Editor

  AutoDuckBase.h

  Markus Meyer

**********************************************************************/
#pragma once

#include "SettingsVisitor.h"
#include "StatefulEffect.h"
#include <cfloat>

class WaveChannel;

class BUILTIN_EFFECTS_API AutoDuckBase : public StatefulEffect
{
public:
    static inline AutoDuckBase* FetchParameters(AutoDuckBase& e, EffectSettings&)
    {
        return &e;
    }

    static const ComponentInterfaceSymbol Symbol;

    AutoDuckBase();
    virtual ~AutoDuckBase();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;

    // Effect implementation

    bool Init() override;
    bool Process(EffectInstance& instance, EffectSettings& settings) override;

private:
    // AutoDuckBase implementation

    bool ApplyDuckFade(int trackNum, WaveChannel& track, double t0, double t1);

protected:
    double mDuckAmountDb;
    double mInnerFadeDownLen;
    double mInnerFadeUpLen;
    double mOuterFadeDownLen;
    double mOuterFadeUpLen;
    double mThresholdDb;
    double mMaximumPause;

    const WaveTrack* mControlTrack {};

    const EffectParameterMethods& Parameters() const override;

    static constexpr EffectParameter DuckAmountDb {
        &AutoDuckBase::mDuckAmountDb, L"DuckAmountDb", -12.0, -24.0, 0.0, 1
    };
    static constexpr EffectParameter InnerFadeDownLen {
        &AutoDuckBase::mInnerFadeDownLen, L"InnerFadeDownLen", 0.0, 0.0, 3.0, 1
    };
    static constexpr EffectParameter InnerFadeUpLen {
        &AutoDuckBase::mInnerFadeUpLen, L"InnerFadeUpLen", 0.0, 0.0, 3.0, 1
    };
    static constexpr EffectParameter OuterFadeDownLen {
        &AutoDuckBase::mOuterFadeDownLen, L"OuterFadeDownLen", 0.5, 0.0, 3.0, 1
    };
    static constexpr EffectParameter OuterFadeUpLen {
        &AutoDuckBase::mOuterFadeUpLen, L"OuterFadeUpLen", 0.5, 0.0, 3.0, 1
    };
    static constexpr EffectParameter ThresholdDb {
        &AutoDuckBase::mThresholdDb, L"ThresholdDb", -30.0, -100.0, 0.0, 1
    };
    static constexpr EffectParameter MaximumPause {
        &AutoDuckBase::mMaximumPause, L"MaximumPause", 1.0, 0.0, DBL_MAX, 1
    };
};
