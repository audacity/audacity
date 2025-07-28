/*
* Audacity: A Digital Audio Editor
*/

#include "modularity/ioc.h"
#include "ui/iinteractiveuriregister.h"

#include "internal/au3/au3exporter.h"
#include "view/exportpreferencesmodel.h"

#include "exportermodule.h"

using namespace au::importexport;
using namespace muse;

static void exporter_init_qrc()
{
    Q_INIT_RESOURCE(exporter);
}

ExporterModule::ExporterModule()
{
}

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

void ExporterModule::resolveImports()
{
    auto ir = ioc()->resolve<muse::ui::IInteractiveUriRegister>(moduleName());
    if (ir) {
        ir->registerQmlUri(Uri("audacity://project/export"), "Export/ExportDialog.qml");
    }
}

void ExporterModule::registerResources()
{
    exporter_init_qrc();
}

void ExporterModule::registerUiTypes()
{
    qmlRegisterType<ExportPreferencesModel>("Audacity.Export", 1, 0, "ExportPreferencesModel");

    qmlRegisterUncreatableType<importexport::ExportChannelsPref>("Audacity.Export", 1, 0, "ExportChannels", "Not creatable from QML");
}

void ExporterModule::onInit(const muse::IApplication::RunMode& mode)
{
    m_configuration->init();
    m_exporter->init();
}
