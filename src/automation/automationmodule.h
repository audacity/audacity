/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/modularity/imodulesetup.h"

namespace au::automation {
class AutomationModule : public muse::modularity::IModuleSetup
{
public:
    AutomationModule();
    ~AutomationModule() override = default;

public:
    std::string moduleName() const override;
    void registerExports() override;
    void registerUiTypes() override;
};
}
