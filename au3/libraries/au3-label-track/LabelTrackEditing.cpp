/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file LabelTrackEditing.cpp

  PaulLicameli

**********************************************************************/
#include "LabelTrack.h"
#include "TempoChange.h"

using OnLabelTrackProjectTempoChange
    =OnProjectTempoChange::Override<LabelTrack>;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(OnLabelTrackProjectTempoChange)
{
    return [](LabelTrack& track, const std::optional<double>& oldTempo,
              double newTempo) {
        if (!oldTempo.has_value()) {
            return;
        }
        const auto ratio = *oldTempo / newTempo;
        const size_t nn = track.GetNumLabels();
        for (size_t ii = 0; ii < nn; ++ii) {
            auto label = *track.GetLabel(ii);
            label.selectedRegion.setTimes(
                label.getT0() * ratio, label.getT1() * ratio);
            track.SetLabel(ii, label);
        }
    };
}
