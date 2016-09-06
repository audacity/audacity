/**********************************************************************

  Audacity: A Digital Audio Editor

  Fade.cpp

  Robert Leidle

*******************************************************************//**

\class EffectFade
\brief An Effect that reduces the volume to zero over  achosen interval.

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/intl.h>

#include "Fade.h"

EffectFade::EffectFade(bool fadeIn)
{
   mFadeIn = fadeIn;
}

EffectFade::~EffectFade()
{
}

// IdentInterface implementation

wxString EffectFade::GetSymbol()
{
   return mFadeIn
      ? FADEIN_PLUGIN_SYMBOL
      : FADEOUT_PLUGIN_SYMBOL;
}

wxString EffectFade::GetDescription()
{
   return mFadeIn
      ? XO("Applies a linear fade-in to the selected audio")
      : XO("Applies a linear fade-out to the selected audio");
}

// EffectIdentInterface implementation

EffectType EffectFade::GetType()
{
   return EffectTypeProcess;
}

bool EffectFade::IsInteractive()
{
   return false;
}

// EffectClientInterface implementation

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
