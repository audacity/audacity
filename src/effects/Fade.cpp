/**********************************************************************

  Audacity: A Digital Audio Editor

  Fade.cpp

  Robert Leidle

*******************************************************************//**

\class EffectFadeIn
\brief An EffectSimpleMono

*//****************************************************************//**

\class EffectFadeOut
\brief An EffectSimpleMono

*//*******************************************************************/

#include "../Audacity.h"

#include "Fade.h"
#include "../WaveTrack.h"

#include <wx/generic/textdlgg.h>
#include <math.h>

bool EffectFadeIn::NewTrackSimpleMono()
{
   mLen = (int)((mCurT1 - mCurT0) * mCurRate + 0.5);
   mSample = 0;

   return true;
}

bool EffectFadeIn::ProcessSimpleMono(float *buffer, sampleCount len)
{
   for (sampleCount i = 0; i < len; i++)
      buffer[i] = (float) (buffer[i] * (float) (mSample + i)
                           / (float) (mLen));
   mSample += len;

   return true;
}

bool EffectFadeOut::NewTrackSimpleMono()
{
   mLen = (int)((mCurT1 - mCurT0) * mCurRate + 0.5);
   mSample = 0;

   return true;
}

bool EffectFadeOut::ProcessSimpleMono(float *buffer, sampleCount len)
{
   for (sampleCount i = 0; i < len; i++)
      buffer[i] = (float) (buffer[i]
                           * (float) (mLen - 1 - (mSample + i))
                           / (float) (mLen));
   mSample += len;

   return true;
}
