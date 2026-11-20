/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>
#include <vector>

#include "framework/global/modularity/imoduleinterface.h"

#include "imacrostephandler.h"

namespace au::macromanager {
class IMacroStepHandlersRegister : MODULE_CONTEXT_INTERFACE
{
    INTERFACE_ID(IMacroStepHandlersRegister)

public:
    virtual ~IMacroStepHandlersRegister() = default;

    virtual void reg(const IMacroStepHandlerPtr& handler) = 0;
    virtual IMacroStepHandlerPtr handler(const std::string& id) const = 0;
    virtual std::vector<IMacroStepHandlerPtr> handlers() const = 0;
};
}
