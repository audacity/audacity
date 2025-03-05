/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../../trackedittypes.h"
#include "libraries/lib-registries/ClientData.h"

class AudacityProject;

namespace au::trackedit {
struct ClipAndTimeSelection {
    const ClipKeyList selectedClips;
    const secs_t dataSelectedStartTime;
    const secs_t dataSelectedEndTime;
};

struct SelectionRestorer : public ClientData::Base
{
    static SelectionRestorer& Get(AudacityProject& project);
    std::function<ClipAndTimeSelection()> selectionGetter;
    std::function<void(const ClipAndTimeSelection&)> selectionSetter;
};
}
