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
    void registerUiTypes() override;

    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& ctx) const override;
};

class AutomationContext : public muse::modularity::IContextSetup
{
public:
    AutomationContext(const muse::modularity::ContextPtr& ctx)
        : muse::modularity::IContextSetup(ctx) {}

    void registerExports() override;
};
}
