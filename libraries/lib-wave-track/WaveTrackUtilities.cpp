/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrackUtilities.cpp

  Paul Licameli

**********************************************************************/
#include "WaveTrackUtilities.h"
#include "BasicUI.h"
#include "UserException.h"
#include "WaveClip.h"
#include <algorithm>

const TranslatableString WaveTrackUtilities::defaultStretchRenderingTitle =
   XO("Pre-processing");

bool WaveTrackUtilities::HasPitchOrSpeed(
   const WaveTrack& track, double t0, double t1)
{
   auto& clips = track.GetClips();
   return any_of(clips.begin(), clips.end(), [&](auto& pClip) {
      return pClip->IntersectsPlayRegion(t0, t1) && pClip->HasPitchOrSpeed();
   });
}

void WaveTrackUtilities::WithClipRenderingProgress(
   std::function<void(const ProgressReporter&)> action,
   TranslatableString title, TranslatableString message)
{
   using namespace BasicUI;
   auto progress =
      MakeProgress(std::move(title), std::move(message), ProgressShowCancel);
   const auto reportProgress = [&](double progressFraction) {
      const auto result = progress->Poll(progressFraction * 1000, 1000);
      if (result != ProgressResult::Success)
         throw UserException {};
   };
   action(reportProgress);
}

bool WaveTrackUtilities::SetClipStretchRatio(
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

void WaveTrackUtilities::ExpandClipTillNextOne(
   const WaveTrack& track, WaveTrack::Interval& interval)
{
   if (
      const auto nextClip =
         track.GetNextClip(*interval.GetClip(0), PlaybackDirection::forward))
   {
      interval.StretchRightTo(nextClip->GetPlayStartTime());
   }
}
