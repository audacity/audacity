/*
* Audacity: A Digital Audio Editor
*/
#include "chirpeffect.h"

#include "global/translation.h"

using namespace au::effects;

const ComponentInterfaceSymbol ChirpEffect::Symbol{ XO("Chirp") };

ChirpEffect::ChirpEffect()
    : ToneGenBase(true)
{}

ComponentInterfaceSymbol ChirpEffect::GetSymbol() const
{
    return Symbol;
}

TranslatableString ChirpEffect::GetDescription() const
{
    return XO("Generates an ascending or descending tone of one of four types");
}

ManualPageID ChirpEffect::ManualPage() const
{
    return L"Chirp";
}

EffectMeta ChirpEffect::meta()
{
    EffectMeta meta;
    meta.title = muse::mtrc("effects", "Chirp");
    meta.categoryId = BUILTIN_CATEGORY_ID;

    return meta;
}
