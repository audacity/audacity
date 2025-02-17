/**********************************************************************

  Audacity: A Digital Audio Editor

  ClickRemovalBase.h

  Craig DeForest

***********************************************************************/
#pragma once

#include "SettingsVisitor.h"
#include "StatefulEffect.h"

class Envelope;
class WaveChannel;

class BUILTIN_EFFECTS_API ClickRemovalBase : public StatefulEffect
{
public:
    static inline ClickRemovalBase*
    FetchParameters(ClickRemovalBase& e, EffectSettings&)
    {
        return &e;
    }

    static const ComponentInterfaceSymbol Symbol;

    ClickRemovalBase();
    virtual ~ClickRemovalBase();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;

    // Effect implementation

    bool CheckWhetherSkipEffect(const EffectSettings& settings) const override;
    bool Process(EffectInstance& instance, EffectSettings& settings) override;

private:
    bool ProcessOne(
        int count, WaveChannel& track, sampleCount start, sampleCount len);

    bool RemoveClicks(size_t len, float* buffer);

protected:
    Envelope* mEnvelope;

    bool mbDidSomething; // This effect usually does nothing on real-world data.
    size_t windowSize;
    int mThresholdLevel;
    int mClickWidth;
    int sep;

    const EffectParameterMethods& Parameters() const override;

    static constexpr EffectParameter Threshold {
        &ClickRemovalBase::mThresholdLevel, L"Threshold", 200, 0, 900, 1
    };
    static constexpr EffectParameter Width {
        &ClickRemovalBase::mClickWidth, L"Width", 20, 0, 40, 1
    };
};
