/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "global/async/notification.h"
#include "modularity/imoduleinterface.h"

#include "types/projectscenetypes.h"

namespace au::projectscene {
class IProjectSceneUiState : MODULE_CONTEXT_INTERFACE
{
    INTERFACE_ID(IProjectSceneUiState)

public:
    virtual ~IProjectSceneUiState() = default;

    virtual TimelineRulerMode timelineRulerMode() const = 0;
    virtual void setTimelineRulerMode(TimelineRulerMode mode) = 0;
    virtual muse::async::Notification timelineRulerModeChanged() const = 0;
};
}
