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

bool TimeStretching::SetClipStretchRatio(
   const WaveTrack& track, WaveTrack::Interval& interval, double stretchRatio)
{
   const auto nextClip =
      track.GetNextInterval(interval, PlaybackDirection::forward);
   const auto maxEndTime = nextClip != nullptr ?
                              nextClip->Start() :
                              std::numeric_limits<double>::infinity();

   const auto start = interval.Start();
   const auto end = interval.End();

   const auto expectedEndTime =
      start + (end - start) * stretchRatio / interval.GetStretchRatio();

   if (expectedEndTime > maxEndTime)
      return false;

   interval.StretchRightTo(expectedEndTime);
   return true;
}

using OnWaveTrackProjectTempoChange = OnProjectTempoChange::Override<WaveTrack>;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(OnWaveTrackProjectTempoChange) {
   return [](WaveTrack &track,
      const std::optional<double> &oldTempo, double newTempo)
   {
      for (const auto pClip : track.Intervals())
         pClip->OnProjectTempoChange(oldTempo, newTempo);
   };
}
