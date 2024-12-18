/*
* Audacity: A Digital Audio Editor
*/
#include "chirpeffect.h"

using namespace au::effects;

const ComponentInterfaceSymbol ChirpEffect::Symbol{ XO("Chirp") };

ChirpEffect::ChirpEffect()
    : ToneEffect(Type::Chirp)
{}

ComponentInterfaceSymbol ChirpEffect::GetSymbol() const
{
    return Symbol;
}
