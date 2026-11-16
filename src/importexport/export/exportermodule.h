/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "modularity/imodulesetup.h"

namespace au::importexport {
class Au3Exporter;
class Au3FFmpegOptionsAccessor;
class ExportConfiguration;
class ExportCommandsController;
class ExporterModule : public muse::modularity::IModuleSetup
{
public:
    ExporterModule();

    std::string moduleName() const override;

    void registerExports() override;
    void resolveImports() override;

    void registerResources() override;
    void registerUiTypes() override;

    void onInit(const muse::IApplication::RunMode& mode) override;

    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& ctx) const override;

private:
    std::shared_ptr<ExportConfiguration> m_configuration;
    std::shared_ptr<Au3FFmpegOptionsAccessor> m_ffmpegOptionsAccessor;
};

class ExporterContext : public muse::modularity::IContextSetup
{
public:
    ExporterContext(const muse::modularity::ContextPtr& ctx)
        : muse::modularity::IContextSetup(ctx) {}

    void registerExports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

private:
    std::shared_ptr<Au3Exporter> m_exporter;
    std::shared_ptr<ExportCommandsController> m_commandsController;
};
}
