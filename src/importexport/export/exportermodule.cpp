/*
* Audacity: A Digital Audio Editor
*/

#include "ui/iinteractiveuriregister.h"

#include "internal/au3/au3exporter.h"
#include "internal/au3/au3ffmpegoptionsaccessor.h"

#include "view/exportpreferencesmodel.h"
#include "view/dynamicexportoptionsmodel.h"
#include "view/customffmpegpreferencesmodel.h"

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
#ifdef AU_USE_FFMPEG
    m_ffmpegOptionsAccessor = std::make_shared<Au3FFmpegOptionsAccessor>();
#endif

    ioc()->registerExport<IExporter>(moduleName(), m_exporter);
    ioc()->registerExport<IExportConfiguration>(moduleName(), m_configuration);
#ifdef AU_USE_FFMPEG
    ioc()->registerExport<IFFmpegOptionsAccessor>(moduleName(), m_ffmpegOptionsAccessor);
#endif
}

void ExporterModule::resolveImports()
{
    auto ir = ioc()->resolve<muse::ui::IInteractiveUriRegister>(moduleName());
    if (ir) {
        ir->registerQmlUri(Uri("audacity://project/export"), "Export/ExportDialog.qml");
        ir->registerQmlUri(Uri("audacity://project/export/ffmpeg"), "Export/CustomFFmpegDialog.qml");
    }
}

void ExporterModule::registerResources()
{
    exporter_init_qrc();
}

void ExporterModule::registerUiTypes()
{
    qmlRegisterType<ExportPreferencesModel>("Audacity.Export", 1, 0, "ExportPreferencesModel");
    qmlRegisterType<DynamicExportOptionsModel>("Audacity.Export", 1, 0, "DynamicExportOptionsModel");
#ifdef AU_USE_FFMPEG
    qmlRegisterType<CustomFFmpegPreferencesModel>("Audacity.Export", 1, 0, "CustomFFmpegPreferencesModel");
#endif

    qmlRegisterUncreatableType<importexport::ExportChannelsPref>("Audacity.Export", 1, 0, "ExportChannels", "Not creatable from QML");
    qmlRegisterUncreatableType<ExportOptionType>("Audacity.Export", 1, 0, "ExportOptionType", "Not creatable from QML");
}

void ExporterModule::onInit(const muse::IApplication::RunMode& mode)
{
    m_configuration->init();
    m_exporter->init();
#ifdef AU_USE_FFMPEG
    m_ffmpegOptionsAccessor->init();
#endif
}
