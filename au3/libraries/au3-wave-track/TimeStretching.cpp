/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TimeStretching.cpp

  Paul Licameli

**********************************************************************/
#include "TimeStretching.h"
#include "au3-basic-ui/BasicUI.h"
#include "SampleBlock.h"
#include "Sequence.h"
#include "au3-stretching-sequence/TempoChange.h"
#include "au3-exceptions/UserException.h"
#include "WaveClip.h"
#include "WaveTrackUtilities.h"
#include <algorithm>

const TranslatableString TimeStretching::defaultStretchRenderingTitle
    =TranslatableString("wave-track", "Pre-processing");

bool TimeStretching::HasPitchOrSpeed(
    const WaveTrack& track, double t0, double t1)
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
                                                  std::move(title), TranslatableString("wave-track", "Rendering Clip"));
}

bool TimeStretching::SetClipStretchRatio(WaveTrack::Interval& interval, double stretchRatio)
{
    //! NOTE: the clip is stretched freely here; resolving any overlap this may
    //! create with a neighbouring clip is the caller's responsibility - matching
    //! how every other clip edit behaves (see Au3ClipsInteraction::makeRoomForClip).
    const auto start = interval.Start();
    const auto end = interval.End();

    const auto expectedEndTime
        =start + (end - start) * stretchRatio / interval.GetStretchRatio();

    interval.StretchRightTo(expectedEndTime);
    return true;
}

using OnWaveTrackProjectTempoChange = OnProjectTempoChange::Override<WaveTrack>;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(OnWaveTrackProjectTempoChange) {
    return [](WaveTrack& track,
              const std::optional<double>& oldTempo, double newTempo)
    {
        for (const auto pClip : track.Intervals()) {
            pClip->OnProjectTempoChange(oldTempo, newTempo);
        }

        if (oldTempo != newTempo) {
            WaveTrackUtilities::RemoveOverlaps(track);
        }
    };
}
