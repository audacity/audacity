/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/async/channel.h"
#include "framework/global/async/notification.h"
#include "framework/actions/actiontypes.h"

namespace au::projectscene {
class ITimelineViewController;
class IPlayPositionViewController;
class IProjectSceneActionsController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IProjectSceneActionsController)

public:
    virtual ~IProjectSceneActionsController() = default;

    virtual void setTimelineViewController(ITimelineViewController* controller) = 0;
    virtual ITimelineViewController* timelineViewController() const = 0;

    virtual void setPlayPositionViewController(IPlayPositionViewController* controller) = 0;
    virtual IPlayPositionViewController* playPositionViewController() const = 0;

    virtual muse::async::Notification effectsPanelFocusRequested() const = 0;
    virtual muse::async::Notification audioSetupContextMenuRequested() const = 0;
    virtual muse::async::Notification timelineContextMenuRequested() const = 0;

    virtual bool actionChecked(const muse::actions::ActionCode& actionCode) const = 0;
    virtual muse::async::Channel<muse::actions::ActionCode> actionCheckedChanged() const = 0;

    virtual muse::async::Channel<muse::actions::ActionCode> actionEnabledChanged() const = 0;
};
}
