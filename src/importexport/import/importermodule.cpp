/*
* Audacity: A Digital Audio Editor
*/

#include "importermodule.h"

#include "modularity/ioc.h"

#include "internal/au3/au3importer.h"

using namespace au::importexport;

ImporterModule::ImporterModule() {}

std::string ImporterModule::moduleName() const
{
    return "importer";
}

void ImporterModule::registerExports()
{
    m_importer = std::make_shared<Au3Importer>();

    ioc()->registerExport<IImporter>(moduleName(), m_importer);
}

void ImporterModule::onInit(const muse::IApplication::RunMode& mode)
{
    m_importer->init();
}
