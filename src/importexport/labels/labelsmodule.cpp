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
using namespace muse;

static void labels_init_qrc()
{
    Q_INIT_RESOURCE(labels);
}

LabelsModule::LabelsModule() {}

std::string LabelsModule::moduleName() const
{
    return "iex_labels";
}

void LabelsModule::registerExports()
{
    m_configuration = std::make_shared<LabelsConfiguration>();

    ioc()->registerExport<ILabelsImporter>(moduleName(), new Au3LabelsImporter(iocContext()));
    ioc()->registerExport<ILabelsExporter>(moduleName(), new Au3LabelsExporter(iocContext()));
    ioc()->registerExport<ILabelsConfiguration>(moduleName(), m_configuration);
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
    auto ir = ioc()->resolve<muse::interactive::IInteractiveUriRegister>(moduleName());
    if (ir) {
        ir->registerQmlUri(Uri("audacity://project/export/labels"), "Export/ExportLabelsDialog.qml");
    }
}

void LabelsModule::onInit(const muse::IApplication::RunMode&)
{
    m_configuration->init();
}
