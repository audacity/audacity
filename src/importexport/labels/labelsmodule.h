/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "modularity/imodulesetup.h"

namespace au::importexport {
class LabelsConfiguration;
class LabelsModule : public muse::modularity::IModuleSetup
{
public:
    LabelsModule();

    std::string moduleName() const override;
    void registerExports() override;
    void registerResources() override;
    void registerUiTypes() override;
    void resolveImports() override;

    void onInit(const muse::IApplication::RunMode& mode) override;

    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& ctx) const override;

private:
    std::shared_ptr<LabelsConfiguration> m_configuration;
};

class LabelsContext : public muse::modularity::IContextSetup
{
public:
    LabelsContext(const muse::modularity::ContextPtr& ctx)
        : muse::modularity::IContextSetup(ctx) {}

    void registerExports() override;
    void onDeinit() override;
};
}
