/*
* Audacity: A Digital Audio Editor
*/
#include "au3wrapmodule.h"

#include <wx/log.h>

#include "mod-ffmpeg/FFmpeg.h"
#include "au3-files/FileNames.h"
#include "au3-import-export/Import.h"
#include "au3-import-export/ExportPluginRegistry.h"
#include "au3-preferences/Prefs.h"
#include "au3-project-file-io/ProjectFileIO.h"
#include "au3-module-manager/ModuleManager.h"

#include "modularity/ioc.h"

#include "internal/wxlogwrap.h"
#include "internal/au3project.h"
#include "internal/au3audiodevicesprovider.h"
#include "internal/au3commonsettings.h"
#include "internal/au3basicui.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include <QStandardPaths>

#include "log.h"

using namespace au::au3;
using namespace muse::modularity;

std::string Au3WrapModule::moduleName() const
{
    return "au3wrap";
}

void Au3WrapModule::registerExports()
{
    m_audioDevicesProvider = std::make_shared<Au3AudioDevicesProvider>();
    m_au3BasicUi = std::make_shared<Au3BasicUI>();

    ioc()->registerExport<IAu3ProjectCreator>(moduleName(), new Au3ProjectCreator());
    ioc()->registerExport<playback::IAudioDevicesProvider>(moduleName(), m_audioDevicesProvider);
}

void Au3WrapModule::onPreInit(const muse::IApplication::RunMode&)
{
    // Make sure the logger is initialized before other modules.
    m_wxLog = new WxLogWrap();
    wxLog::SetActiveTarget(m_wxLog);

    std::unique_ptr<Au3CommonSettings> auset = std::make_unique<Au3CommonSettings>();
    InitPreferences(std::move(auset));
}

void Au3WrapModule::onInit(const muse::IApplication::RunMode&)
{
    bool ok = ProjectFileIO::InitializeSQL();
    if (!ok) {
        LOGE() << "failed init sql";
    }

    m_audioDevicesProvider->init();

    FFmpegStartup();

    ModuleManager::Get().Initialize();
    Importer::Get().Initialize();
    ExportPluginRegistry::Get().Initialize();

    muse::String tempDir = projectConfiguration()->temporaryDir().toString();
    UpdateDefaultPath(FileNames::Operation::Temp, wxFromString(tempDir));

    (void)BasicUI::Install(m_au3BasicUi.get());
}

void Au3WrapModule::onDeinit()
{
    (void)BasicUI::Install(nullptr);

    wxLog::SetActiveTarget(nullptr);
    delete m_wxLog;
}
