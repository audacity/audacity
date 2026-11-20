/*
* Audacity: A Digital Audio Editor
*/
#include "macrostephandlersregister.h"

#include <algorithm>

using namespace au::macromanager;

void MacroStepHandlersRegister::reg(const IMacroStepHandlerPtr& handler)
{
    m_handlers.push_back(handler);
}

IMacroStepHandlerPtr MacroStepHandlersRegister::handler(const std::string& id) const
{
    const auto it = std::ranges::find(m_handlers, id, &IMacroStepHandler::id);
    return it != m_handlers.end() ? *it : nullptr;
}

std::vector<IMacroStepHandlerPtr> MacroStepHandlersRegister::handlers() const
{
    return m_handlers;
}
