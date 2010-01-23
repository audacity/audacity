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

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 0f9cf46d-6aea-48a6-ab44-c95277a1467d

