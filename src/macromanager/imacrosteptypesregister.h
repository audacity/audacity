/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>
#include <vector>

#include "framework/global/modularity/imoduleinterface.h"

#include "imacrosteptype.h"

namespace au::macromanager {
class IMacroStepTypesRegister : MODULE_CONTEXT_INTERFACE
{
    INTERFACE_ID(IMacroStepTypesRegister)

public:
    virtual ~IMacroStepTypesRegister() = default;

    virtual void reg(const IMacroStepTypePtr& type) = 0;
    virtual IMacroStepTypePtr type(const std::string& id) const = 0;
    virtual std::vector<IMacroStepTypePtr> types() const = 0;
};
}
