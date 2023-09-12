/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrackUtilities.cpp

  Paul Licameli

**********************************************************************/
#include "WaveTrackUtilities.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include <algorithm>

bool WaveTrackUtilities::HasStretch(
   const WaveTrack &track, double t0, double t1)
{
   auto &clips = track.GetClips();
   return any_of(clips.begin(), clips.end(),
      [&](auto &pClip){
         return pClip->IntersectsPlayRegion(t0, t1) &&
            pClip->GetStretchRatio() != 1.0;
      });
}
