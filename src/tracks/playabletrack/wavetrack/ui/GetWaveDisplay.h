/**********************************************************************

  Audacity: A Digital Audio Editor

  @file GetWaveDisplay.h

  Paul Licameli split from Sequence.h

**********************************************************************/

#ifndef __AUDACITY_GET_WAVE_DISPLAY__
#define __AUDACITY_GET_WAVE_DISPLAY__

#include <cstddef>
class Sequence;
class sampleCount;

// where is input, assumed to be nondecreasing, and its size is len + 1.
// min, max, rms, bl are outputs, and their lengths are len.
// Each position in the output arrays corresponds to one column of pixels.
// The column for pixel p covers samples from
// where[p] up to (but excluding) where[p + 1].
// bl is negative wherever data are not yet available.
// Return true if successful.
bool GetWaveDisplay(const Sequence &sequence,
   float *min, float *max, float *rms, int* bl,
   size_t len, const sampleCount *where);

#endif
