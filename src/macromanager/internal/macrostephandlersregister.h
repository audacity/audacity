/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include "macromanager/imacrostephandlersregister.h"

namespace au::macromanager {
class MacroStepHandlersRegister : public IMacroStepHandlersRegister
{
public:
    void reg(const IMacroStepHandlerPtr& handler) override;
    IMacroStepHandlerPtr handler(const std::string& id) const override;
    std::vector<IMacroStepHandlerPtr> handlers() const override;

private:
    std::vector<IMacroStepHandlerPtr> m_handlers;
};
}
