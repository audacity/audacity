/*
* Audacity: A Digital Audio Editor
*/
#include "chirpeffect.h"

using namespace au::effects;

const ComponentInterfaceSymbol ChirpEffect::Symbol{ XO("Chirp") };

ChirpEffect::ChirpEffect()
    : ToneGenBase(true)
{}

ComponentInterfaceSymbol ChirpEffect::GetSymbol() const
{
    return Symbol;
}
