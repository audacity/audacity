/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include "macromanager/imacrosteptypesregister.h"

namespace au::macromanager {
class MacroStepTypesRegister : public IMacroStepTypesRegister
{
public:
    void reg(const IMacroStepTypePtr& type) override;
    IMacroStepTypePtr type(const std::string& id) const override;
    std::vector<IMacroStepTypePtr> types() const override;

private:
    std::vector<IMacroStepTypePtr> m_types;
};
}
