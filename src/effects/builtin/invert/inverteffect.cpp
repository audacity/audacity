#include "inverteffect.h"
#include "LoadEffects.h"

namespace au::effects {
InvertEffect::InvertEffect()
{
}

InvertEffect::~InvertEffect()
{
}

const ComponentInterfaceSymbol InvertEffect::Symbol { XO("Invert") };

// ComponentInterface implementation

ComponentInterfaceSymbol InvertEffect::GetSymbol() const
{
    return Symbol;
}

TranslatableString InvertEffect::GetDescription() const
{
    return XO("Flips the audio samples upside-down, reversing their polarity");
}

// EffectDefinitionInterface implementation

EffectType InvertEffect::GetType() const
{
    return EffectTypeProcess;
}

bool InvertEffect::IsInteractive() const
{
    return false;
}

unsigned InvertEffect::GetAudioInCount() const
{
    return 1;
}

unsigned InvertEffect::GetAudioOutCount() const
{
    return 1;
}

size_t InvertEffect::ProcessBlock(
    EffectSettings&, const float* const* inBlock, float* const* outBlock,
    size_t blockLen)
{
    const float* ibuf = inBlock[0];
    float* obuf = outBlock[0];

    for (decltype(blockLen) i = 0; i < blockLen; i++) {
        obuf[i] = -ibuf[i];
    }

    return blockLen;
}

bool InvertEffect::NeedsDither() const
{
    return false;
}
}
