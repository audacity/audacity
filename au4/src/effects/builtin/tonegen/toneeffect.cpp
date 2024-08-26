/*
* Audacity: A Digital Audio Editor
*/
#include "toneeffect.h"

using namespace au::effects;

const ComponentInterfaceSymbol ToneEffect::Symbol{ XO("Tone") };

ToneEffect::ToneEffect()
    : ToneGenBase(false)
{}

ComponentInterfaceSymbol ToneEffect::GetSymbol() const
{
    return Symbol;
}
