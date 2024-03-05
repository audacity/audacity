/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TimeStretching.cpp

  Paul Licameli

**********************************************************************/
#include "TimeStretching.h"
#include "BasicUI.h"
#include "SampleBlock.h"
#include "Sequence.h"
#include "TempoChange.h"
#include "UserException.h"
#include "WaveClip.h"
#include <algorithm>

const TranslatableString TimeStretching::defaultStretchRenderingTitle =
   XO("Pre-processing");

bool TimeStretching::HasPitchOrSpeed(
   const WaveTrack &track, double t0, double t1)
{
   auto clips = track.Intervals();
   return std::any_of(clips.begin(), clips.end(), [&](auto pClip) {
      return pClip->IntersectsPlayRegion(t0, t1) && pClip->HasPitchOrSpeed();
   });
}

void TimeStretching::WithClipRenderingProgress(
   std::function<void(const ProgressReporter&)> action,
   const TranslatableString title)
{
   return UserException::WithCancellableProgress(move(action),
      std::move(title), XO("Rendering Clip"));
}

using OnWaveTrackProjectTempoChange = OnProjectTempoChange::Override<WaveTrack>;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(OnWaveTrackProjectTempoChange) {
   return [](WaveTrack &track,
      const std::optional<double> &oldTempo, double newTempo)
   {
      assert(track.IsLeader());
      for (const auto pClip : track.Intervals())
         pClip->OnProjectTempoChange(oldTempo, newTempo);
   };
}
