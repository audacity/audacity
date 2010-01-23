/**********************************************************************

  Audacity: A Digital Audio Editor

  Spectrum.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_SPECTRUM__
#define __AUDACITY_SPECTRUM__

#include "WaveTrack.h"

/*
  This function computes the power (mean square amplitude) as
  a function of frequency, for some block of audio data.

  width: the number of samples
  calculates windowSize/2 frequency samples
*/

bool ComputeSpectrum(float * data, int width, int windowSize,
                     double rate, float *out, bool autocorrelation, int windowFunc=3);

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: a3211706-7547-4b7a-b970-1ac937be7ace

