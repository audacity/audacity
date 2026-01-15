/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../../trackedittypes.h"
#include "au3-registries/ClientData.h"

class AudacityProject;

namespace au::trackedit {
struct ClipAndTimeSelection {
    const TrackIdList selectedTracks;
    const ClipKeyList selectedClips;
    const LabelKeyList selectedLabels;
    const secs_t dataSelectedStartTime = 0.;
    const secs_t dataSelectedEndTime = 0.;
    // TODO frequency selection
};

struct SelectionRestorer : public ClientData::Base
{
    static SelectionRestorer& Get(AudacityProject& project);
    std::function<ClipAndTimeSelection()> selectionGetter;
    std::function<void(const ClipAndTimeSelection&)> selectionSetter;
};
}
