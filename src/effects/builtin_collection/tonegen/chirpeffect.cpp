/*
* Audacity: A Digital Audio Editor
*/
#include "chirpeffect.h"

#include "au3-strings/TranslatableString.h"

using namespace au::effects;

const ComponentInterfaceSymbol ChirpEffect::Symbol{ TranslatableString("effects-tonegen", "Chirp") };

ChirpEffect::ChirpEffect()
    : ToneEffect(Type::Chirp)
{}

ComponentInterfaceSymbol ChirpEffect::GetSymbol() const
{
    return Symbol;
}
