/*
* Audacity: A Digital Audio Editor
*/
#include "au3wrapmodule.h"

#include <wx/log.h>

#include "FFmpeg.h"
#include "FileNames.h"
#include "libraries/lib-audio-io/AudioIO.h"
#include "libraries/lib-import-export/Import.h"
#include "libraries/lib-import-export/ExportPluginRegistry.h"
#include "libraries/lib-preferences/Prefs.h"
#include "libraries/lib-project-file-io/ProjectFileIO.h"
#include "libraries/lib-module-manager/ModuleManager.h"

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

void Au3WrapModule::onInit(const muse::IApplication::RunMode&)
{
    m_wxLog = new WxLogWrap();
    wxLog::SetActiveTarget(m_wxLog);

    std::unique_ptr<Au3CommonSettings> auset = std::make_unique<Au3CommonSettings>();
    InitPreferences(std::move(auset));

    AudioIO::Init();

    bool ok = ProjectFileIO::InitializeSQL();
    if (!ok) {
        LOGE() << "failed init sql";
    }

    m_audioDevicesProvider->init();

#ifdef AU_USE_FFMPEG
    FFmpegStartup();
#endif

    ModuleManager::Get().Initialize();
    Importer::Get().Initialize();
    ExportPluginRegistry::Get().Initialize();

    muse::String tempDir = projectConfiguration()->temporaryDir().toString();
    UpdateDefaultPath(FileNames::Operation::Temp, wxFromString(tempDir));

    (void)BasicUI::Install(m_au3BasicUi.get());
}

void Au3WrapModule::onDeinit()
{
    AudioIO::Deinit();
    (void)BasicUI::Install(nullptr);

    wxLog::SetActiveTarget(nullptr);
    delete m_wxLog;
}
