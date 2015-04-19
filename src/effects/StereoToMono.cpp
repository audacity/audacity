/**********************************************************************

  Audacity: A Digital Audio Editor

  StereoToMono.cpp

  Lynn Allan

*******************************************************************//**

\class EffectStereoToMono
\brief An Effect.

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/intl.h>

#include "StereoToMono.h"

EffectStereoToMono::EffectStereoToMono()
{
}

EffectStereoToMono::~EffectStereoToMono()
{
}

// IdentInterface implementation

wxString EffectStereoToMono::GetSymbol()
{
   return STEREOTOMONO_PLUGIN_SYMBOL;
}

wxString EffectStereoToMono::GetDescription()
{
   return XO("Converts stereo tracks to mono");
}

// EffectIdentInterface implementation

EffectType EffectStereoToMono::GetType()
{
   // Really EffectTypeProcess, but this prevents it from showing in the Effect Menu
   return EffectTypeNone;
}

bool EffectStereoToMono::IsInteractive()
{
   return false;
}

// EffectClientInterface implementation

int EffectStereoToMono::GetAudioInCount()
{
   return 2;
}

int EffectStereoToMono::GetAudioOutCount()
{
   return 1;
}

sampleCount EffectStereoToMono::ProcessBlock(float **inBlock, float **outBlock, sampleCount blockLen)
{
   float *left = inBlock[0];
   float *right = inBlock[1];
   float *mono = outBlock[0];

   for (sampleCount i = 0; i < blockLen; i++)
   {
      mono[i] = (left[i] + right[i]) / 2.0;
   }

   return blockLen;
}

// Effect implementatino

bool EffectStereoToMono::IsHidden()
{
   return true;
}

