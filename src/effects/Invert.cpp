/**********************************************************************

  Audacity: A Digital Audio Editor

  Invert.cpp

  Mark Phillips


*******************************************************************//**

\class EffectInvert
\brief An EffectSimpleMono that inverts the selected audio.

*//*******************************************************************/


#include "../Audacity.h"

#include "Invert.h"

bool EffectInvert::ProcessSimpleMono(float *buffer, sampleCount len)
{
   sampleCount i;
   for (i = 0; i < len; i++)
      buffer[i] = -buffer[i];
   return true;
}

