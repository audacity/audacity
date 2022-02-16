/**********************************************************************

  Audacity: A Digital Audio Editor

  @file GetWaveDisplay.h

  Paul Licameli split from Sequence.h

**********************************************************************/

#ifndef __AUDACITY_GET_WAVE_DISPLAY__
#define __AUDACITY_GET_WAVE_DISPLAY__

#include <cstddef>
class Sequence;
struct WaveDisplayColumn;
class PixelSampleMapper;

// where is input, assumed to be nondecreasing, and its size is len + 1.
// min, max, rms, and their lengths are len.
// Each position in the output arrays corresponds to one column of pixels.
// The column for pixel p covers samples from
// where[p] up to (but excluding) where[p + 1].
// Return true if successful.
bool GetWaveDisplay(
   const Sequence& sequence, const PixelSampleMapper& mapper,
   WaveDisplayColumn* columns, size_t len);

#endif
