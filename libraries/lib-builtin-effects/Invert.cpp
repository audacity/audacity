/**********************************************************************

  Audacity: A Digital Audio Editor

  Invert.cpp

  Mark Phillips

  This class inverts the selected audio.

**********************************************************************/
#include "Invert.h"
#include "LoadEffects.h"

Invert::Invert()
{
}

Invert::~Invert()
{
}

const ComponentInterfaceSymbol Invert::Symbol { XO("Invert") };

// ComponentInterface implementation

ComponentInterfaceSymbol Invert::GetSymbol() const
{
   return Symbol;
}

TranslatableString Invert::GetDescription() const
{
   return XO("Flips the audio samples upside-down, reversing their polarity");
}

// EffectDefinitionInterface implementation

EffectType Invert::GetType() const
{
   return EffectTypeProcess;
}

bool Invert::IsInteractive() const
{
   return false;
}

unsigned Invert::GetAudioInCount() const
{
   return 1;
}

unsigned Invert::GetAudioOutCount() const
{
   return 1;
}

size_t Invert::ProcessBlock(
   EffectSettings&, const float* const* inBlock, float* const* outBlock,
   size_t blockLen)
{
   const float* ibuf = inBlock[0];
   float* obuf = outBlock[0];

   for (decltype(blockLen) i = 0; i < blockLen; i++)
   {
      obuf[i] = -ibuf[i];
   }

   return blockLen;
}

bool Invert::NeedsDither() const
{
   return false;
}
