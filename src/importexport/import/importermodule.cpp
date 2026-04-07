/*
* Audacity: A Digital Audio Editor
*/

#include "importermodule.h"

#include "modularity/ioc.h"

#include "internal/au3/au3importer.h"
#include "internal/importerconfiguration.h"

using namespace au::importexport;

static const std::string mname("importer");

ImporterModule::ImporterModule() {}

std::string ImporterModule::moduleName() const
{
    return mname;
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
    m_configuration = std::make_shared<ImporterConfiguration>();

    ioc()->registerExport<IImporter>(mname, m_importer);
    ioc()->registerExport<IImporterConfiguration>(mname, m_configuration);
}

void ImporterContext::onInit(const muse::IApplication::RunMode&)
{
    m_configuration->init();
    m_importer->init();
}

void ImporterContext::onDeinit()
{
}
