#pragma once

#include "StatefulPerTrackEffect.h"

namespace au::effects {
class FadeEffectBase : public StatefulPerTrackEffect
{
public:
    FadeEffectBase(bool fadeIn = false);
    virtual ~FadeEffectBase() override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;
    bool IsInteractive() const override;

    unsigned GetAudioInCount() const override;
    unsigned GetAudioOutCount() const override;
    bool ProcessInitialize(
        EffectSettings& settings, double sampleRate, ChannelNames chanMap) override;
    size_t ProcessBlock(
        EffectSettings& settings, const float* const* inBlock, float* const* outBlock, size_t blockLen) override;

protected:
    // EffectFade implementation

    bool mFadeIn;
    sampleCount mSample;
};

class FadeInEffect final : public FadeEffectBase
{
public:
    static const ComponentInterfaceSymbol Symbol;

    FadeInEffect();

    // ComponentInterface implementation
    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
};

class FadeOutEffect final : public FadeEffectBase
{
public:
    static const ComponentInterfaceSymbol Symbol;

    FadeOutEffect();

    // ComponentInterface implementation
    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
};
}
