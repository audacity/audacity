/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file NoteTrackEditing.cpp

  PaulLicameli

**********************************************************************/
#include "WrapAllegro.h"
#include "NoteTrack.h"
#include "TempoChange.h"

using OnNoteTrackProjectTempoChange = OnProjectTempoChange::Override<NoteTrack>;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(OnNoteTrackProjectTempoChange) {
    return [](NoteTrack& track,
              const std::optional<double>& oldTempo, double newTempo)
    {
        if (!oldTempo.has_value()) {
            return;
        }
        const auto ratio = *oldTempo / newTempo;
        auto& seq = track.GetSeq();
        seq.convert_to_beats();
        const auto b1 = seq.get_dur();
        seq.convert_to_seconds();
        const auto newDuration = seq.get_dur() * ratio;
        seq.stretch_region(0, b1, newDuration);
        seq.set_real_dur(newDuration);
    };
}
