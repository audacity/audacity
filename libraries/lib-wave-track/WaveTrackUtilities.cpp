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
#include "WaveTrack.h"
#include <algorithm>

const TranslatableString WaveTrackUtilities::defaultStretchRenderingTitle =
   XO("Pre-processing");

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

void WaveTrackUtilities::WithStretchRenderingProgress(
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
