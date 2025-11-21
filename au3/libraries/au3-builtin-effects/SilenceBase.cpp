/**********************************************************************

  Audacity: A Digital Audio Editor

  SilenceBase.cpp

  Dominic Mazzoni

*******************************************************************//**

\class SilenceBase
\brief An effect to add silence.

*//*******************************************************************/
#include "SilenceBase.h"
#include "WaveTrack.h"

const ComponentInterfaceSymbol SilenceBase::Symbol
/* i18n-hint: noun */
{ XC("Silence", "generator") };

SilenceBase::SilenceBase()
{
    SetLinearEffectFlag(true);
}

SilenceBase::~SilenceBase()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol SilenceBase::GetSymbol() const
{
    return Symbol;
}

TranslatableString SilenceBase::GetDescription() const
{
    return XO("Creates audio of zero amplitude");
}

ManualPageID SilenceBase::ManualPage() const
{
    return L"Silence";
}

// EffectDefinitionInterface implementation

EffectType SilenceBase::GetType() const
{
    return EffectTypeGenerate;
}

bool SilenceBase::GenerateTrack(const EffectSettings& settings, WaveTrack& tmp)
{
    tmp.InsertSilence(0.0, settings.extra.GetDuration());
    return true;
}
