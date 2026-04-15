/*
 * Audacity: A Digital Audio Editor
 */
#include "nyquistprompteffect.h"
#include "au3-module-manager/PluginManager.h" // NYQUIST_PROMPT_ID

namespace au::effects {
const ComponentInterfaceSymbol NyquistPromptEffect::Symbol { NYQUIST_PROMPT_ID };

NyquistPromptEffect::NyquistPromptEffect()
    : NyquistBase(NYQUIST_PROMPT_ID) {}

ComponentInterfaceSymbol NyquistPromptEffect::GetSymbol() const
{
    return Symbol;
}

EffectFamilySymbol NyquistPromptEffect::GetFamily() const
{
    return { wxT("Audacity"), XO("Built-in") };
}
}
