/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"
#include "async/channel.h"
#include "global/progress.h"
#include "actions/actiontypes.h"

namespace au::projectscene {
class IProjectSceneActionsController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IProjectSceneActionsController)

public:
    virtual ~IProjectSceneActionsController() = default;

    virtual bool actionChecked(const muse::actions::ActionCode& actionCode) const = 0;
    virtual muse::async::Channel<muse::actions::ActionCode> actionCheckedChanged() const = 0;
};
}
