/**********************************************************************

  Audacity: A Digital Audio Editor

  SilenceBase.cpp

  Dominic Mazzoni

*******************************************************************//**

\class SilenceBase
\brief An effect to add silence.

*//*******************************************************************/
#include "SilenceBase.h"
#include "au3-wave-track/WaveTrack.h"

const ComponentInterfaceSymbol SilenceBase::Symbol
/*: noun */
{ TranslatableString("builtin-effects", "Silence", "generator") };

namespace {
// According to https://manual.audacityteam.org/man/silence.html,
// generating silence with an audio selection should behave like
// the "Silence Audio" command, which doesn't affect track clip
// boundaries.
constexpr auto preserveUnderlyingClipBoundaries = true;
}

SilenceBase::SilenceBase()
    : Generator(preserveUnderlyingClipBoundaries)
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
    return TranslatableString("builtin-effects", "Creates audio of zero amplitude");
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
