#ifndef AU_PROJECT_PROJECTACTIONSCONTROLLER_H
#define AU_PROJECT_PROJECTACTIONSCONTROLLER_H

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/global/io/ifilesystem.h"

#include "framework/actions/actionable.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/interactive/iinteractive.h"
#include "framework/interactive/iplatforminteractive.h"
#include "framework/ui/imainwindow.h"

#include "context/iglobalcontext.h"
#include "project/irecentfilescontroller.h"
#include "iopensaveprojectscenario.h"
#include "toast/itoastservice.h"
#include "trackedit/iprojecthistory.h"
#include "record/irecordcontroller.h"
#include "importexport/export/internal/exportconfiguration.h"
#include "importexport/import/iimporter.h"
#include "au3cloud/iau3audiocomservice.h"
#include "au3cloud/iauthorization.h"

#include "project/iprojectconfiguration.h"
#include "project/iprojectfilescontroller.h"
#include "project/iaudacityproject.h"
#include "multiwindows/imultiwindowsprovider.h"

namespace au::project {
class ProjectActionsController : public IProjectFilesController, public muse::actions::Actionable, public muse::async::Asyncable,
    public muse::Contextable
{
    muse::GlobalInject<IProjectConfiguration> configuration;
    muse::GlobalInject<muse::io::IFileSystem> fileSystem;
    muse::GlobalInject<importexport::ExportConfiguration> exportConfiguration;
    muse::GlobalInject<muse::IPlatformInteractive> platformInteractive;
    muse::GlobalInject<IRecentFilesController> recentFilesController;
    muse::GlobalInject<muse::mi::IMultiWindowsProvider> multiwindowsProvider;
    muse::GlobalInject<toast::IToastService> toastService;
    muse::GlobalInject<au3cloud::IAuthorization> authorization;

    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher { this };
    muse::ContextInject<muse::ui::IMainWindow> mainWindow { this };
    muse::ContextInject<au::context::IGlobalContext> globalContext { this };
    muse::ContextInject<muse::IInteractive> interactive { this };
    muse::ContextInject<IOpenSaveProjectScenario> openSaveProjectScenario { this };
    muse::ContextInject<trackedit::IProjectHistory> projectHistory { this };
    muse::ContextInject<record::IRecordController> recordController { this };
    muse::ContextInject<importexport::IImporter> importer { this };
    muse::ContextInject<au3cloud::IAu3AudioComService> audioComService { this };

public:
    ProjectActionsController(muse::modularity::ContextPtr ctx = nullptr);

    void init();

    bool canReceiveAction(const muse::actions::ActionCode& code) const override;

    [[nodiscard]] muse::Ret openProject(const ProjectFile& file) override;
    bool isUrlSupported(const QUrl& url) const override;
    bool isFileSupported(const muse::io::path_t& path) const override;
    bool closeOpenedProject(bool quitApp = false) override;
    bool saveProject(const muse::io::path_t& path = muse::io::path_t()) override;
    bool saveProjectLocally(const muse::io::path_t& filePath = muse::io::path_t(), SaveMode saveMode = SaveMode::Save) override;
    bool saveProjectToCloud(const CloudProjectInfo& cloudInfo, SaveMode saveMode = SaveMode::Save, bool forceOverwrite = false) override;

    const ProjectBeingDownloaded& projectBeingDownloaded() const override;
    muse::async::Notification projectBeingDownloadedChanged() const override;

    bool isProjectOpened(const muse::io::path_t& projectPath) const;
    const muse::actions::ActionCodeList& prohibitedActionsWhileRecording() const;

private:
    project::IAudacityProjectPtr currentProject() const;

    void newProject();
    void open(const muse::actions::ActionData& args);
    void openCloudProject(const muse::actions::ActionData& args);
    void importFiles(const muse::actions::ActionData& args);

    void importStartupMedia(const muse::actions::ActionData& args);
    muse::Ret processMediaFiles(const muse::io::paths_t& paths);

    muse::Ret openProject(const muse::io::path_t& path,
                          const muse::String& displayNameOverride = muse::String(), const muse::String& projectId = muse::String());
    muse::Ret loadWithFallback(const IAudacityProjectPtr& project, const muse::io::path_t& loadPath, const std::string& format);
    muse::Ret doOpenProject(const muse::io::path_t& filePath);
    IAudacityProjectPtr createProjectInCurrentWindow();
    muse::Ret openCloudProject(const muse::io::path_t& localPath, const muse::String& projectId, bool forceOverwrite = false);
    //! TODO AU4
    // muse::Ret openAudacityUrl(const QUrl& url);
    muse::RetVal<IAudacityProjectPtr> loadProject(const muse::io::path_t& filePath);
    muse::io::paths_t selectOpeningFiles();
    muse::io::paths_t selectImportFiles();

    bool shouldRetryLoadAfterError(const muse::Ret& ret, const muse::io::path_t& filepath);
    void warnProjectCannotBeOpened(const muse::Ret& ret, const muse::io::path_t& filepath) const;

    muse::IInteractive::Button askAboutSavingProject(IAudacityProjectPtr project);

    muse::Ret canSaveProject() const;
    bool saveProject(SaveMode saveMode, SaveLocationType saveLocationType = SaveLocationType::Undefined, bool force = false);
    bool saveProjectAt(const SaveLocation& saveLocation, SaveMode saveMode = SaveMode::Save, bool force = false);

    RecentFile makeRecentFile(IAudacityProjectPtr project);
    void clearRecentProjects();

    void exportAudio();
    void exportLabels(const muse::actions::ActionData& args);
    void exportMIDI();

    void undo();
    void redo();

    muse::Ret openPageIfNeed(muse::Uri pageUri);

    void handleCloudOpenError(const muse::Ret& error, const muse::io::path_t& localPath, const std::string& cloudProjectId = {});
    void handleCloudAudioOpenError(const muse::Ret& error);
    void handleCloudSaveError(const muse::Ret& error);

    void shareAudio();
    void openCloudAudioFile(const muse::actions::ActionQuery& query);

    void openCustomFFmpegOptions();
    void openMetadataDialog();
    void openCustomMapping();

    muse::Ret ensureAuthorization();

    bool m_isProjectSaving = false;
    bool m_isProjectClosing = false;
    bool m_isProjectProcessing = false;

    ProjectBeingDownloaded m_projectBeingDownloaded;
    muse::async::Notification m_projectBeingDownloadedChanged;
    muse::async::Channel<muse::actions::ActionCodeList> m_actionEnabledChanged;
};
}

#endif // AU_PROJECT_PROJECTACTIONSCONTROLLER_H
