/*
* Audacity: A Digital Audio Editor
*/

#include "framework/interactive/iinteractiveuriregister.h"

#include "internal/au3/au3exporter.h"
#include "internal/au3/au3ffmpegoptionsaccessor.h"

#include "view/exportpreferencesmodel.h"
#include "view/dynamicexportoptionsmodel.h"
#include "view/customffmpegpreferencesmodel.h"
#include "view/metadatamodel.h"
#include "view/custommappingmodel.h"
#include "view/channelmappingtableviewmodel.h"

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
    m_exporter = std::make_shared<Au3Exporter>(iocContext());
    m_configuration = std::make_shared<ExportConfiguration>();
    m_ffmpegOptionsAccessor = std::make_shared<Au3FFmpegOptionsAccessor>();

    ioc()->registerExport<IExporter>(moduleName(), m_exporter);
    ioc()->registerExport<IExportConfiguration>(moduleName(), m_configuration);
    ioc()->registerExport<IFFmpegOptionsAccessor>(moduleName(), m_ffmpegOptionsAccessor);
}

void ExporterModule::resolveImports()
{
    auto ir = ioc()->resolve<muse::interactive::IInteractiveUriRegister>(moduleName());
    if (ir) {
        ir->registerQmlUri(Uri("audacity://project/export"), "Export/ExportDialog.qml");
        ir->registerQmlUri(Uri("audacity://project/export/ffmpeg"), "Export/CustomFFmpegDialog.qml");
        ir->registerQmlUri(Uri("audacity://project/export/metadata"), "Export/MetadataDialog.qml");
        ir->registerQmlUri(Uri("audacity://project/export/mapping"), "Export/CustomMappingDialog.qml");
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
    qmlRegisterType<CustomFFmpegPreferencesModel>("Audacity.Export", 1, 0, "CustomFFmpegPreferencesModel");
    qmlRegisterType<MetadataModel>("Audacity.Export", 1, 0, "MetadataModel");
    qmlRegisterType<CustomMappingModel>("Audacity.Export", 1, 0, "CustomMappingModel");
    qmlRegisterType<ChannelMappingTableViewModel>("Audacity.Export", 1, 0, "ChannelMappingTableViewModel");
    qmlRegisterUncreatableMetaObject(ChannelMappingTableViewCellType::staticMetaObject,
                                     "Audacity.Export", 1, 0, "ChannelMappingTableViewCellType", "");

    qmlRegisterUncreatableType<importexport::ExportChannelsPref>("Audacity.Export", 1, 0, "ExportChannels", "Not creatable from QML");
    qmlRegisterUncreatableType<ExportOptionType>("Audacity.Export", 1, 0, "ExportOptionType", "Not creatable from QML");
}

void ExporterModule::onInit(const muse::IApplication::RunMode&)
{
    m_ffmpegOptionsAccessor->init();
    m_configuration->init();
    m_exporter->init();
}
