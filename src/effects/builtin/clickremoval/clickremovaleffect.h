/*
 * Audacity: A Digital Audio Editor
 */

/**********************************************************************

  Audacity: A Digital Audio Editor

  ClickRemovalEffect.h

  Craig DeForest

***********************************************************************/
#pragma once

#include "SettingsVisitor.h"
#include "StatefulEffect.h"

class Envelope;
class WaveChannel;

namespace au::effects {
class ClickRemovalEffect : public StatefulEffect
{
public:
    static inline ClickRemovalEffect*
    FetchParameters(ClickRemovalEffect& e, EffectSettings&)
    {
        return &e;
    }

    static const ComponentInterfaceSymbol Symbol;

    ClickRemovalEffect();
    virtual ~ClickRemovalEffect();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    ::EffectType GetType() const override;

    // Effect implementation

    bool CheckWhetherSkipEffect(const EffectSettings& settings) const override;
    bool Process(::EffectInstance& instance, EffectSettings& settings) override;

private:
    bool ProcessOne(
        int count, WaveChannel& track, sampleCount start, sampleCount len);

    bool RemoveClicks(size_t len, float* buffer);

private:
    Envelope* mEnvelope;

    bool mbDidSomething; // This effect usually does nothing on real-world data.

public:
    int mThresholdLevel;
    int mClickWidth;
    int sep;

    const EffectParameterMethods& Parameters() const override;

    static constexpr EffectParameter Threshold {
        &ClickRemovalEffect::mThresholdLevel, L"Threshold", 200, 0, 900, 10
    };
    static constexpr EffectParameter Width {
        &ClickRemovalEffect::mClickWidth, L"Width", 20, 0, 40, 10
    };
};
}
