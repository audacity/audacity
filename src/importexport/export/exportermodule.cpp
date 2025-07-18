/*
* Audacity: A Digital Audio Editor
*/

#include "modularity/ioc.h"
#include "ui/iinteractiveuriregister.h"

#include "internal/au3/au3exporter.h"

#include "exportermodule.h"

using namespace au::importexport;

ExporterModule::ExporterModule() {}

std::string ExporterModule::moduleName() const
{
    return "exporter";
}

void ExporterModule::registerExports()
{
    m_exporter = std::make_shared<Au3Exporter>();
    m_configuration = std::make_shared<ExportConfiguration>();

    ioc()->registerExport<IExporter>(moduleName(), m_exporter);
    ioc()->registerExport<IExportConfiguration>(moduleName(), m_configuration);
}

void ExporterModule::onInit(const muse::IApplication::RunMode& mode)
{
    m_configuration->init();
}
