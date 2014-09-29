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
