/*
* Audacity: A Digital Audio Editor
*/
#include "macrosteptypesregister.h"

#include <algorithm>

using namespace au::macromanager;

void MacroStepTypesRegister::reg(const IMacroStepTypePtr& type)
{
    m_types.push_back(type);
}

IMacroStepTypePtr MacroStepTypesRegister::type(const std::string& id) const
{
    const auto it = std::ranges::find(m_types, id, &IMacroStepType::id);
    return it != m_types.end() ? *it : nullptr;
}

std::vector<IMacroStepTypePtr> MacroStepTypesRegister::types() const
{
    return m_types;
}
