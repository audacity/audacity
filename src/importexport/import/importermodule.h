/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "modularity/imodulesetup.h"

namespace au::importexport {
class Au3Importer;
class ImporterConfiguration;

class ImporterModule : public muse::modularity::IModuleSetup
{
public:
    ImporterModule();

    std::string moduleName() const override;

    void registerExports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;

    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& ctx) const override;

private:
    std::shared_ptr<ImporterConfiguration> m_configuration;
};

class ImporterContext : public muse::modularity::IContextSetup
{
public:
    ImporterContext(const muse::modularity::ContextPtr& ctx)
        : muse::modularity::IContextSetup(ctx) {}

    void registerExports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

private:
    std::shared_ptr<Au3Importer> m_importer;
};
}
