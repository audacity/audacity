/*
* Audacity: A Digital Audio Editor
*/

#include "labelsmodule.h"

#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iinteractiveuriregister.h"

#include "internal/au3/au3labelsimporter.h"
#include "internal/au3/au3labelsexporter.h"

#include "internal/labelsconfiguration.h"

#include "view/exportlabelsmodel.h"

using namespace au::importexport;

static const std::string mname("iex_labels");

static void labels_init_qrc()
{
    Q_INIT_RESOURCE(labels);
}

LabelsModule::LabelsModule() {}

std::string LabelsModule::moduleName() const
{
    return mname;
}

void LabelsModule::registerExports()
{
    m_configuration = std::make_shared<LabelsConfiguration>();

    globalIoc()->registerExport<ILabelsConfiguration>(mname, m_configuration);
}

void LabelsModule::registerResources()
{
    labels_init_qrc();
}

void LabelsModule::registerUiTypes()
{
    qmlRegisterType<ExportLabelsModel>("Audacity.Export", 1, 0, "ExportLabelsModel");
}

void LabelsModule::resolveImports()
{
    auto ir = globalIoc()->resolve<muse::interactive::IInteractiveUriRegister>(mname);
    if (ir) {
        ir->registerQmlUri(muse::Uri("audacity://project/export/labels"), "Export/ExportLabelsDialog.qml");
    }
}

void LabelsModule::onInit(const muse::IApplication::RunMode&)
{
    m_configuration->init();
}

muse::modularity::IContextSetup* LabelsModule::newContext(const muse::modularity::ContextPtr& ctx) const
{
    return new LabelsContext(ctx);
}

// =====================================================
// LabelsContext
// =====================================================

void LabelsContext::registerExports()
{
    ioc()->registerExport<ILabelsImporter>(mname, new Au3LabelsImporter(iocContext()));
    ioc()->registerExport<ILabelsExporter>(mname, new Au3LabelsExporter(iocContext()));
}

void LabelsContext::onDeinit()
{
}
