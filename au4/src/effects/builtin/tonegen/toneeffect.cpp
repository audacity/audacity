/*
* Audacity: A Digital Audio Editor
*/
#include "toneeffect.h"

#include "global/translation.h"

using namespace au::effects;

const ComponentInterfaceSymbol ToneEffect::Symbol{ XO("Tone") };

ToneEffect::ToneEffect()
    : ToneGenBase(false)
{}

ComponentInterfaceSymbol ToneEffect::GetSymbol() const
{
    return Symbol;
}

TranslatableString ToneEffect::GetDescription() const
{
    return XO("Generates a constant frequency tone of one of four types");
}

ManualPageID ToneEffect::ManualPage() const
{
    return L"Tone";
}

EffectMeta ToneEffect::meta()
{
    EffectMeta meta;
    meta.title = muse::mtrc("effects", "Tone");
    meta.categoryId = BUILTIN_CATEGORY_ID;

    return meta;
}
