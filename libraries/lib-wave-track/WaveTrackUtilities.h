/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrackUtilities.h

  Paul Licameli

  @brief some convenient iterations over clips, needing only the public
 interface of WaveTrack

**********************************************************************/

#include "Internat.h"
#include "TranslatableString.h"

class WaveTrack;
using ProgressReporter = std::function<void(double)>;

namespace WaveTrackUtilities {

//! Whether any clips, whose play regions intersect the interval, have non-unit
//! stretch ratio
WAVE_TRACK_API
bool HasStretch(const WaveTrack &track, double t0, double t1);

extern WAVE_TRACK_API const TranslatableString defaultStretchRenderingTitle;

WAVE_TRACK_API void WithStretchRenderingProgress(
   std::function<void(const ProgressReporter&)> action,
   TranslatableString title = defaultStretchRenderingTitle,
   TranslatableString message = XO("Rendering Time-Stretched Audio"));
}
