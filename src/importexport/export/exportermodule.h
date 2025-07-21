/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "modularity/imodulesetup.h"

#include "internal/exportconfiguration.h"

namespace au::importexport {
class Au3Exporter;
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

private:
    std::shared_ptr<Au3Exporter> m_exporter;
    std::shared_ptr<ExportConfiguration> m_configuration;
};
}
