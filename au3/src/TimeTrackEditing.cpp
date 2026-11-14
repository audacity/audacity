/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file TimeTrackEditing.cpp

  PaulLicameli

**********************************************************************/
#include "Envelope.h"
#include "TempoChange.h"
#include "TimeTrack.h"

using OnTimeTrackProjectTempoChange = OnProjectTempoChange::Override<TimeTrack>;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(OnTimeTrackProjectTempoChange) {
    return [](TimeTrack& track,
              const std::optional<double>& oldTempo, double newTempo)
    {
        if (!oldTempo.has_value()) {
            return;
        }
        const auto ratio = *oldTempo / newTempo;
        track.GetEnvelope()->RescaleTimesBy(ratio);
    };
}
