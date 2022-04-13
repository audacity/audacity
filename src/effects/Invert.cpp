/**********************************************************************

  Audacity: A Digital Audio Editor

  Invert.cpp

  Mark Phillips


*******************************************************************//**

\class EffectInvert
\brief An Effect that inverts the selected audio.

*//*******************************************************************/


#include "Invert.h"

#include <wx/intl.h>

#include "LoadEffects.h"

const ComponentInterfaceSymbol EffectInvert::Symbol
{ XO("Invert") };

namespace{ BuiltinEffectsModule::Registration< EffectInvert > reg; }

EffectInvert::EffectInvert()
{
}

EffectInvert::~EffectInvert()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectInvert::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectInvert::GetDescription() const
{
   return XO("Flips the audio samples upside-down, reversing their polarity");
}

// EffectDefinitionInterface implementation

EffectType EffectInvert::GetType() const
{
   return EffectTypeProcess;
}

bool EffectInvert::IsInteractive() const
{
   return false;
}

unsigned EffectInvert::GetAudioInCount() const
{
   return 1;
}

unsigned EffectInvert::GetAudioOutCount() const
{
   return 1;
}

size_t EffectInvert::ProcessBlock(EffectSettings &,
   const float *const *inBlock, float *const *outBlock, size_t blockLen)
{
   const float *ibuf = inBlock[0];
   float *obuf = outBlock[0];

   for (decltype(blockLen) i = 0; i < blockLen; i++)
   {
      obuf[i] = -ibuf[i];
   }

   return blockLen;
}
