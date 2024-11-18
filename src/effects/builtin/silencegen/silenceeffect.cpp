/*
* Audacity: A Digital Audio Editor
*/
#include "silenceeffect.h"

using namespace au::effects;

const ComponentInterfaceSymbol SilenceEffect::Symbol{ XO("Silence") };

SilenceEffect::SilenceEffect()
    : SilenceBase(), GeneratorEffect(mProjectRate, mT0, mT1)
{}

ComponentInterfaceSymbol SilenceEffect::GetSymbol() const
{
    return Symbol;
}

void SilenceEffect::doInit()
{}
