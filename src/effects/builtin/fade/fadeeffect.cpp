#include "fadeeffect.h"

namespace au::effects {
FadeEffectBase::FadeEffectBase(bool fadeIn)
{
    mFadeIn = fadeIn;
}

FadeEffectBase::~FadeEffectBase()
{
}

// EffectDefinitionInterface implementation

EffectType FadeEffectBase::GetType() const
{
    return EffectTypeProcess;
}

bool FadeEffectBase::IsInteractive() const
{
    return false;
}

unsigned FadeEffectBase::GetAudioInCount() const
{
    return 1;
}

unsigned FadeEffectBase::GetAudioOutCount() const
{
    return 1;
}

bool FadeEffectBase::ProcessInitialize(EffectSettings&, double, ChannelNames)
{
    mSample = 0;
    return true;
}

size_t FadeEffectBase::ProcessBlock(
    EffectSettings&, const float* const* inBlock, float* const* outBlock,
    size_t blockLen)
{
    const float* ibuf = inBlock[0];
    float* obuf = outBlock[0];

    if (mFadeIn) {
        for (decltype(blockLen) i = 0; i < blockLen; i++) {
            obuf[i] = (ibuf[i] * (mSample++).as_float()) / mSampleCnt.as_float();
        }
    } else {
        for (decltype(blockLen) i = 0; i < blockLen; i++) {
            obuf[i] = (ibuf[i] * (mSampleCnt - 1 - mSample++).as_float())
                      / mSampleCnt.as_float();
        }
    }

    return blockLen;
}

const ComponentInterfaceSymbol FadeInEffect::Symbol { XO("Fade In") };

FadeInEffect::FadeInEffect()
    : FadeEffectBase{true}
{
}

ComponentInterfaceSymbol FadeInEffect::GetSymbol() const
{
    return Symbol;
}

TranslatableString FadeInEffect::GetDescription() const
{
    return XO("Applies a linear fade-in to the selected audio");
}

const ComponentInterfaceSymbol FadeOutEffect::Symbol { XO("Fade Out") };

FadeOutEffect::FadeOutEffect()
    : FadeEffectBase{false}
{
}

ComponentInterfaceSymbol FadeOutEffect::GetSymbol() const
{
    return Symbol;
}

TranslatableString FadeOutEffect::GetDescription() const
{
    return XO("Applies a linear fade-out to the selected audio");
}
}
