/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "global/async/channel.h"
#include "global/modularity/imoduleinterface.h"

namespace au::appshell {
class IGuiFocusState : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IGuiFocusState)

public:
    virtual ~IGuiFocusState() = default;

    virtual bool isFocused() const = 0;
    virtual muse::async::Channel<bool> isFocusedChanged() const = 0;
};
}
