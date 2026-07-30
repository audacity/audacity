/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"
#include "async/channel.h"
#include "actions/actiontypes.h"

namespace au::trackedit {
class ITrackeditActionsController
{
public:
    virtual ~ITrackeditActionsController() = default;

    virtual bool actionEnabled(const muse::actions::ActionCode& act) const = 0;
    //! NOTE: batched — one notification may carry many action codes, so that a
    //! state change affecting dozens of actions triggers a single UI update
    virtual muse::async::Channel<muse::actions::ActionCodeList> actionEnabledChanged() const = 0;

    virtual bool actionChecked(const muse::actions::ActionCode& actionCode) const = 0;
    virtual muse::async::Channel<muse::actions::ActionCode> actionCheckedChanged() const = 0;
};
}
