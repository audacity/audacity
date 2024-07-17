#pragma once

#include "modularity/imoduleinterface.h"

#include "global/async/channel.h"

#include "processing/processingtypes.h"

namespace au::projectscene {
class IPlayCursorController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(projectscene::IPlayCursorController)

public:
    virtual ~IPlayCursorController() = default;

    virtual processing::secs_t timePosition() const = 0;
    virtual void setTimePosition(processing::secs_t newTimePosition) = 0;
    virtual muse::async::Channel<processing::secs_t> timePositionChanged() const = 0;
};
}
