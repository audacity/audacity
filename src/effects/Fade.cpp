/**********************************************************************

  Audacity: A Digital Audio Editor

  Fade.cpp

  Robert Leidle

*******************************************************************//**

\class EffectFade
\brief An Effect that reduces the volume to zero over  achosen interval.

*//*******************************************************************/


#include "Fade.h"

#include <wx/intl.h>

#include "LoadEffects.h"

const ComponentInterfaceSymbol EffectFadeIn::Symbol
{ XO("Fade In") };

namespace{ BuiltinEffectsModule::Registration< EffectFadeIn > reg; }

const ComponentInterfaceSymbol EffectFadeOut::Symbol
{ XO("Fade Out") };

namespace{ BuiltinEffectsModule::Registration< EffectFadeOut > reg2; }

EffectFade::EffectFade(bool fadeIn)
{
   mFadeIn = fadeIn;
}

EffectFade::~EffectFade()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectFade::GetSymbol()
{
   return mFadeIn
      ? EffectFadeIn::Symbol
      : EffectFadeOut::Symbol;
}

TranslatableString EffectFade::GetDescription()
{
   return mFadeIn
      ? XO("Applies a linear fade-in to the selected audio")
      : XO("Applies a linear fade-out to the selected audio");
}

// EffectDefinitionInterface implementation

EffectType EffectFade::GetType()
{
   return EffectTypeProcess;
}

bool EffectFade::IsInteractive()
{
   return false;
}

// EffectProcessor implementation

unsigned EffectFade::GetAudioInCount()
{
   return 1;
}

unsigned EffectFade::GetAudioOutCount()
{
   return 1;
}

bool EffectFade::ProcessInitialize(sampleCount WXUNUSED(totalLen), ChannelNames WXUNUSED(chanMap))
{
   mSample = 0;

   return true;
}

size_t EffectFade::ProcessBlock(float **inBlock, float **outBlock, size_t blockLen)
{
   float *ibuf = inBlock[0];
   float *obuf = outBlock[0];

   if (mFadeIn)
   {
      for (decltype(blockLen) i = 0; i < blockLen; i++)
      {
         obuf[i] =
            (ibuf[i] * ( mSample++ ).as_float()) /
            mSampleCnt.as_float();
      }
   }
   else
   {
      for (decltype(blockLen) i = 0; i < blockLen; i++)
      {
         obuf[i] = (ibuf[i] *
                    ( mSampleCnt - 1 - mSample++ ).as_float()) /
            mSampleCnt.as_float();
      }
   }

   return blockLen;
}
