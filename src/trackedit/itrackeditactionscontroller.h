/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"
#include "async/channel.h"
#include "actions/actiontypes.h"

namespace au::trackedit {
class ITrackeditActionsController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ITrackeditActionsController)

public:
    virtual ~ITrackeditActionsController() = default;

    virtual bool actionChecked(const muse::actions::ActionCode& actionCode) const = 0;
    virtual muse::async::Channel<muse::actions::ActionCode> actionCheckedChanged() const = 0;
};
}
