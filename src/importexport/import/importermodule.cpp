/*
* Audacity: A Digital Audio Editor
*/

#include "importermodule.h"

#include "modularity/ioc.h"

#include "internal/au3/au3importer.h"
#include "internal/importerconfiguration.h"

#include "RegisterImportPlugins.h" // from au3/modules/import-export/ see IMPORT_EXPORT_MODULE in au3wrapDefs.cmake

using namespace au::importexport;

static const std::string mname("importer");

ImporterModule::ImporterModule()
{
    RegisterImportPlugins();
}

std::string ImporterModule::moduleName() const
{
    return mname;
}

void ImporterModule::registerExports()
{
    m_configuration = std::make_shared<ImporterConfiguration>();

    globalIoc()->registerExport<IImporterConfiguration>(mname, m_configuration);
}

void ImporterModule::onInit(const muse::IApplication::RunMode&)
{
    m_configuration->init();
}

muse::modularity::IContextSetup* ImporterModule::newContext(const muse::modularity::ContextPtr& ctx) const
{
    return new ImporterContext(ctx);
}

// =====================================================
// ImporterContext
// =====================================================

void ImporterContext::registerExports()
{
    m_importer = std::make_shared<Au3Importer>(iocContext());

    ioc()->registerExport<IImporter>(mname, m_importer);
}

void ImporterContext::onInit(const muse::IApplication::RunMode&)
{
}

void ImporterContext::onDeinit()
{
}
