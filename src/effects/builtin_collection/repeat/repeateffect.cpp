/*
 * Audacity: A Digital Audio Editor
 */
#include "repeateffect.h"

#include "au3-strings/TranslatableString.h"

#include <algorithm>

namespace au::effects {
const ComponentInterfaceSymbol RepeatEffect::Symbol { TranslatableString("effects-repeat", "Repeat") };

RepeatEffect::RepeatEffect()
{
}

RepeatEffect::~RepeatEffect()
{
}

ComponentInterfaceSymbol RepeatEffect::GetSymbol() const
{
    return Symbol;
}

bool RepeatEffect::Process(::EffectInstance& instance, EffectSettings& settings)
{
    // Also covers presets and scripted use, which don't go through the dialog
    repeatCount = std::min(repeatCount, MaxRepeatCount);
    return RepeatBase::Process(instance, settings);
}
}
