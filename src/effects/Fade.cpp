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

int EffectFade::GetAudioInCount()
{
   return 1;
}

int EffectFade::GetAudioOutCount()
{
   return 1;
}

bool EffectFade::ProcessInitialize(sampleCount WXUNUSED(totalLen), ChannelNames WXUNUSED(chanMap))
{
   mSample = 0;

   return true;
}

sampleCount EffectFade::ProcessBlock(float **inBlock, float **outBlock, sampleCount blockLen)
{
   float *ibuf = inBlock[0];
   float *obuf = outBlock[0];

   if (mFadeIn)
   {
      for (sampleCount i = 0; i < blockLen; i++)
      {
         obuf[i] = (ibuf[i] * ((float) mSample++)) / mSampleCnt;
      }
   }
   else
   {
      for (sampleCount i = 0; i < blockLen; i++)
      {
         obuf[i] = (ibuf[i] * ((float) mSampleCnt - 1 - mSample++)) / mSampleCnt;
      }
   }

   return blockLen;
}
