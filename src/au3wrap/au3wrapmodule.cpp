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
#include "au3-project/Project.h"
#include "au3-project-file-io/ProjectFileIO.h"
#include "au3-module-manager/ModuleManager.h"

#include "context/iglobalcontext.h"
#include "project/iaudacityproject.h"

#include "modularity/ioc.h"

#include "internal/wxlogwrap.h"
#include "internal/au3project.h"
#include "internal/au3commonsettings.h"
#include "internal/au3basicui.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include <QStandardPaths>

#include "log.h"

using namespace au::au3;
using namespace muse::modularity;

static const std::string mname("au3wrap");

std::string Au3WrapModule::moduleName() const
{
    return mname;
}

void Au3WrapModule::registerExports()
{
    globalIoc()->registerExport<IAu3ProjectCreator>(moduleName(), new Au3ProjectCreator());
}

void Au3WrapModule::onPreInit(const muse::IApplication::RunMode&)
{
    // Make sure the logger is initialized before other modules.
    // Note: ownership is transferred to wxLog via SetActiveTarget.
    // AudacityLogger::Get() may later replace and delete this target,
    // so we only keep a non-owning pointer to check identity in onDeinit.
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

    FFmpegStartup();

    muse::String tempDir = projectConfiguration()->temporaryDir().toString();
    UpdateDefaultPath(FileNames::Operation::Temp, wxFromString(tempDir));

    m_au3BasicUi = std::make_shared<Au3BasicUI>(application.get());
    (void)BasicUI::Install(m_au3BasicUi.get());

    // Map AudacityProject to context for WindowPlacement
    auto app = application.get();
    static WindowPlacementFactory::Scope scope {
        [app](AudacityProject& project) -> std::unique_ptr<const BasicUI::WindowPlacement> {
            auto projectAddr = reinterpret_cast<uintptr_t>(&project);
            for (const auto& ctx : app->contexts()) {
                auto gc = muse::modularity::ioc(ctx)->resolve<au::context::IGlobalContext>("au3wrap");
                if (!gc) {
                    continue;
                }
                auto prj = gc->currentProject();
                if (prj && prj->au3ProjectPtr() == projectAddr) {
                    return std::make_unique<Au3WindowPlacement>(ctx);
                }
            }
            return std::make_unique<BasicUI::WindowPlacement>();
        }
    };
}

void Au3WrapModule::onAllInited(const muse::IApplication::RunMode& mode)
{
    ModuleManager::Get().Initialize();
    Importer::Get().Initialize();
    ExportPluginRegistry::Get().Initialize();
}

void Au3WrapModule::onDeinit()
{
    (void)BasicUI::Install(nullptr);
    m_au3BasicUi.reset();

    if (m_wxLog) {
        // Only clean up if it's still the active target;
        // if something else replaced it, that code took ownership
        if (wxLog::GetActiveTarget() == m_wxLog) {
            delete wxLog::SetActiveTarget(nullptr);
        }
        m_wxLog = nullptr;
    }
}
