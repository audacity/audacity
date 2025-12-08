/*
* Audacity: A Digital Audio Editor
*/

#include "importermodule.h"

#include "modularity/ioc.h"

#include "internal/au3/au3importer.h"
#include "internal/au3/au3labelsimporter.h"
#include "internal/importconfiguration.h"

using namespace au::importexport;

ImporterModule::ImporterModule() {}

std::string ImporterModule::moduleName() const
{
    return "importer";
}

void ImporterModule::registerExports()
{
    m_importer = std::make_shared<Au3Importer>();
    m_configuration = std::make_shared<ImportConfiguration>();

    ioc()->registerExport<IImporter>(moduleName(), m_importer);
    ioc()->registerExport<ILabelsImporter>(moduleName(), new Au3LabelsImporter());
    ioc()->registerExport<IImportConfiguration>(moduleName(), m_configuration);
}

void ImporterModule::onInit(const muse::IApplication::RunMode&)
{
    m_importer->init();
    m_configuration->init();
}
