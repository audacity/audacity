#ifndef AU_PROJECT_PROJECTACTIONSCONTROLLER_H
#define AU_PROJECT_PROJECTACTIONSCONTROLLER_H

#include "actions/actionable.h"
#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "actions/iactionsdispatcher.h"
#include "context/iglobalcontext.h"
#include "global/iinteractive.h"
#include "project/iprojectconfiguration.h"
#include "global/io/ifilesystem.h"
#include "project/irecentfilescontroller.h"
#include "iopensaveprojectscenario.h"
#include "trackedit/iprojecthistory.h"
#include "record/irecordcontroller.h"
#include "importexport/export/internal/exportconfiguration.h"

#include "project/iprojectfilescontroller.h"
#include "project/iaudacityproject.h"

namespace au::project {
class ProjectActionsController : public IProjectFilesController, public muse::actions::Actionable, public muse::async::Asyncable,
    public muse::Injectable
{
    muse::GlobalInject<IProjectConfiguration> configuration;
    muse::GlobalInject<muse::io::IFileSystem> fileSystem;
    muse::GlobalInject<importexport::ExportConfiguration> exportConfiguration;

    muse::GlobalInject<IRecentFilesController> recentFilesController;

    muse::Inject<muse::actions::IActionsDispatcher> dispatcher { this };
    muse::Inject<au::context::IGlobalContext> globalContext { this };
    muse::Inject<muse::IInteractive> interactive { this };
    muse::Inject<IOpenSaveProjectScenario> openSaveProjectScenario { this };
    muse::Inject<trackedit::IProjectHistory> projectHistory { this };
    muse::Inject<record::IRecordController> recordController { this };

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

    const ProjectBeingDownloaded& projectBeingDownloaded() const override;
    muse::async::Notification projectBeingDownloadedChanged() const override;

    bool isProjectOpened(const muse::io::path_t& projectPath) const;
    const muse::actions::ActionCodeList& prohibitedActionsWhileRecording() const;

private:
    project::IAudacityProjectPtr currentProject() const;

    void newProject();
    void openProject(const muse::actions::ActionData& args);
    void importFile();
    muse::Ret openProject(const muse::io::path_t& givenPath, const muse::String& displayNameOverride = muse::String());
    muse::Ret loadWithFallback(const IAudacityProjectPtr& project, const muse::io::path_t& loadPath, const std::string& format);
    muse::Ret doOpenProject(const muse::io::path_t& filePath);
    //! TODO AU4
    // muse::Ret openAudacityUrl(const QUrl& url);
    muse::RetVal<IAudacityProjectPtr> loadProject(const muse::io::path_t& filePath);
    muse::io::path_t selectOpeningFile();
    muse::io::path_t selectImportFile();

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

    void openCustomFFmpegOptions();
    void openMetadataDialog();

    bool m_isProjectSaving = false;
    bool m_isProjectClosing = false;
    bool m_isProjectProcessing = false;

    ProjectBeingDownloaded m_projectBeingDownloaded;
    muse::async::Notification m_projectBeingDownloadedChanged;
    muse::async::Channel<muse::actions::ActionCodeList> m_actionEnabledChanged;
};
}

#endif // AU_PROJECT_PROJECTACTIONSCONTROLLER_H
