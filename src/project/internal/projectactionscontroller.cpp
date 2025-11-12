#include "projectactionscontroller.h"

#include <QFileDialog>

#include "async/async.h"
#include "global/defer.h"
#include "global/translation.h"

#include "audacityproject.h"
#include "projecterrors.h"

#include "project/types/projecttypes.h"

#include "log.h"

using namespace muse;
using namespace au::project;

static const muse::Uri PROJECT_PAGE_URI("audacity://project");
static const muse::Uri HOME_PAGE_URI("musescore://home");
static const muse::Uri NEW_PROJECT_URI("audacity://project/new");

static const muse::Uri EXPORT_URI("audacity://project/export");
static const muse::Uri CUSTOM_FFMPEG_OPTIONS("audacity://project/export/ffmpeg");
static const muse::Uri METADATA_DIALOG_URI("audacity://project/export/metadata");

static const QString AUDACITY_URL_SCHEME("audacity");
static const QString OPEN_PROJECT_URL_HOSTNAME("open-project");

static const muse::actions::ActionCode OPEN_CUSTOM_FFMPEG_OPTIONS("open-custom-ffmpeg-options");
static const muse::actions::ActionCode OPEN_METADATA_DIALOG("open-metadata-dialog");

void ProjectActionsController::init()
{
    dispatcher()->reg(this, "file-new", this, &ProjectActionsController::newProject);
    dispatcher()->reg(this, "file-open", this, &ProjectActionsController::openProject);
    dispatcher()->reg(this, "clear-recent", this, &ProjectActionsController::clearRecentProjects);
    dispatcher()->reg(this, "project-import", this, &ProjectActionsController::importFile);

    dispatcher()->reg(this, "file-save", [this]() { saveProject(SaveMode::Save); });
    //! TODO AU4: decide whether to implement these functions from scratch in AU4 or
    //! to install our own implementation of the UI (BasicUI API)
    //! right now there's only BasicUI stub which means there's no progress dialog shown on saving
    dispatcher()->reg(this, "file-save-as", [this]() { saveProject(SaveMode::SaveAs); });
    dispatcher()->reg(this, "file-save-backup", [this]() { saveProject(SaveMode::SaveCopy); });

    dispatcher()->reg(this, "export-audio", this, &ProjectActionsController::exportAudio);
    dispatcher()->reg(this, "export-labels", this, &ProjectActionsController::exportLabels);
    dispatcher()->reg(this, "export-midi", this, &ProjectActionsController::exportMIDI);

    dispatcher()->reg(this, "file-close", [this]() {
        // reset preferred export sample rate
        exportConfiguration()->setExportSampleRate(-1);

        //! TODO AU4
        bool quitApp = false; //multiInstancesProvider()->instances().size() > 1;
        closeOpenedProject(quitApp);
    });

    dispatcher()->reg(this, OPEN_CUSTOM_FFMPEG_OPTIONS, this, &ProjectActionsController::openCustomFFmpegOptions);
    dispatcher()->reg(this, OPEN_METADATA_DIALOG, this, &ProjectActionsController::openMetadataDialog);
}

const muse::actions::ActionCodeList& ProjectActionsController::prohibitedActionsWhileRecording() const
{
    static const std::vector<muse::actions::ActionCode> PROHIBITED_WHILE_RECORDING {
        "file-new",
        "file-open",
        "file-close",
        "project-import",
        "file-save",
        "file-save-as",
        "file-save-backup",
        "export-audio",
        "export-labels",
        "export-midi",
    };

    return PROHIBITED_WHILE_RECORDING;
}

bool ProjectActionsController::canReceiveAction(const muse::actions::ActionCode& code) const
{
    if (!currentProject()) {
        static const std::unordered_set<actions::ActionCode> DONT_REQUIRE_OPEN_PROJECT {
            "file-new",
            "file-open",
            "continue-last-session",
            "clear-recent",
        };

        return muse::contains(DONT_REQUIRE_OPEN_PROJECT, code);
    } else if (recordController()->isRecording()) {
        return !muse::contains(prohibitedActionsWhileRecording(), code);
    }

    return true;
}

IAudacityProjectPtr ProjectActionsController::currentProject() const
{
    return globalContext()->currentProject();
}

Ret ProjectActionsController::openProject(const ProjectFile& file)
{
    LOGI() << "Try open project: url = " << file.url.toString() << ", displayNameOverride = " << file.displayNameOverride;

    if (file.isNull() || file.url.isLocalFile()) {
        muse::io::path_t filename = file.isNull() ? selectOpeningFile() : file.path();

        if (filename.empty()) {
            return make_ret(Ret::Code::Cancel);
        }

        if (au::project::isAudacity3File(filename)) {
            auto resolved = openSaveProjectScenario()->resolveLegacyProjectFormat(filename);
            if (!resolved.ret) {
                return resolved.ret;
            }
            filename = resolved.val;
        }

        return openProject(filename, file.displayNameOverride);
    }

    //! TODO: Fix me
    // if (file.url.scheme() == AUDACITY_URL_SCHEME) {
    //     return openMuseScoreUrl(file.url);
    // }

    return make_ret(Err::UnsupportedUrl);
}

void ProjectActionsController::newProject()
{
    //! NOTE This method is synchronous,
    //! but inside `multiInstancesProvider` there can be an event loop
    //! to wait for the responses from other instances, accordingly,
    //! the events (like user click) can be executed and this method can be called several times,
    //! before the end of the current call.
    //! So we ignore all subsequent calls until the current one completes.
    if (m_isProjectProcessing) {
        return;
    }
    m_isProjectProcessing = true;

    DEFER {
        m_isProjectProcessing = false;
    };

    if (globalContext()->currentProject()) {
#ifdef MU_BUILD_MULTIINSTANCE_MODULE
        //! Check, if any project is already open in the current window
        //! and there is already a created instance without a project, then activate it
        if (multiInstancesProvider()->isHasAppInstanceWithoutProject()) {
            multiInstancesProvider()->activateWindowWithoutProject();
            return;
        }

        //! Otherwise, we will create a new instance
        QStringList args;
        args << "--session-type" << "start-with-new";
        multiInstancesProvider()->openNewAppInstance(args);
#else
        LOGE() << "Has current project, but no multiinstance module, create new unable, need close current";
#endif
        return;
    }

    IAudacityProjectPtr project = std::make_shared<Audacity4Project>();
    project->createNew();

    globalContext()->setCurrentProject(project);

    projectHistory()->init();

    openPageIfNeed(PROJECT_PAGE_URI);
}

void ProjectActionsController::openProject(const muse::actions::ActionData& args)
{
    UNUSED(args);
    const QUrl url = !args.empty() ? args.arg<QUrl>(0) : QUrl();
    const QString displayNameOverride = args.count() >= 2 ? args.arg<QString>(1) : QString();

    Ret ret = openProject(ProjectFile(url, displayNameOverride));
    if (!ret) {
        openPageIfNeed(HOME_PAGE_URI);
    }
}

void ProjectActionsController::importFile()
{
    const muse::io::path_t askedPath = selectImportFile();
    const IAudacityProjectPtr project = globalContext()->currentProject();
    project->import(askedPath);
}

bool ProjectActionsController::isUrlSupported(const QUrl& url) const
{
    if (url.isLocalFile()) {
        return isFileSupported(muse::io::path_t(url));
    }

    if (url.scheme() == AUDACITY_URL_SCHEME) {
        if (url.host() == OPEN_PROJECT_URL_HOSTNAME) {
            return true;
        }
    }

    return false;
}

bool ProjectActionsController::isFileSupported(const muse::io::path_t& path) const
{
    if (au::project::isAudacityFile(path)) {
        return true;
    }

    return false;
}

bool ProjectActionsController::closeOpenedProject(const bool quitApp)
{
    if (m_isProjectClosing) {
        return false;
    }

    m_isProjectClosing = true;
    DEFER {
        m_isProjectClosing = false;
    };

    const IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return true;
    }

    bool result = true;

    if (project->hasUnsavedChanges()) {
        IInteractive::Button btn = askAboutSavingProject(project);

        if (btn == IInteractive::Button::Cancel) {
            result = false;
        } else if (btn == IInteractive::Button::Save) {
            result = saveProject();
        } else if (btn == IInteractive::Button::DontSave) {
            result = true;
        }
    }

    if (result) {
        interactive()->closeAllDialogs();

        project->close();

        globalContext()->setCurrentProject(nullptr);

        if (quitApp) {
            //! NOTE: we need to call `quit` in the next event loop due to controlling the lifecycle of this method
            muse::async::Async::call(this, [this](){
                dispatcher()->dispatch("quit", actions::ActionData::make_arg1<bool>(false));
            });
        } else {
            Ret ret = openPageIfNeed(HOME_PAGE_URI);
            if (!ret) {
                LOGE() << ret.toString();
            }
        }
    }

    return result;
}

bool ProjectActionsController::saveProject(const muse::io::path_t& path)
{
    if (!path.empty()) {
        return saveProjectLocally(path);
    }

    return saveProject(SaveMode::Save);
}

bool ProjectActionsController::saveProjectLocally(const muse::io::path_t& filePath, SaveMode saveMode)
{
    IAudacityProjectPtr project = currentProject();
    if (!project) {
        return false;
    }

    Ret ret = project->save(filePath, saveMode);
    //! TODO AU4
    // if (!ret) {
    //     LOGE() << ret.toString();
    //     warnScoreCouldnotBeSaved(ret);
    //     return false;
    // }

    recentFilesController()->prependRecentFile(makeRecentFile(project));
    return true;
}

const ProjectBeingDownloaded& ProjectActionsController::projectBeingDownloaded() const
{
    return m_projectBeingDownloaded;
}

async::Notification ProjectActionsController::projectBeingDownloadedChanged() const
{
    return m_projectBeingDownloadedChanged;
}

muse::io::path_t ProjectActionsController::selectOpeningFile()
{
    std::string allExt = "*.aup3 *.aup4";

    std::vector<std::string> filter { trc("project", "All supported files") + " (" + allExt + ")",
                                      trc("project", "Audacity 3 files") + " (*.aup3)",
                                      trc("project", "Audacity 4 files") + " (*.aup4)" };

    io::path_t defaultDir = configuration()->lastOpenedProjectsPath();

    if (defaultDir.empty()) {
        defaultDir = configuration()->userProjectsPath();
    }

    if (defaultDir.empty()) {
        defaultDir = configuration()->defaultUserProjectsPath();
    }

    io::path_t filePath = interactive()->selectOpeningFileSync(muse::trc("project", "Open"), defaultDir, filter);

    if (!filePath.empty()) {
        configuration()->setLastOpenedProjectsPath(io::dirpath(filePath));
    }

    return filePath;
}

io::path_t ProjectActionsController::selectImportFile()
{
    std::string audioFileExt
        = "*.aac *.ac3 *.mp3 *.wma *.wav *.flac *.ogg *.opus *.aif *.aiff *.amr *.ape *.au *.dts *.mpc *.tta *.wv *.shn *.voc *.mmf";
    std::string videoFileExt
        = "*.avi *.mp4 *.mkv *.mov *.flv *.wmv *.asf *.webm *.mpg *.mpeg *.m4v *.ts *.gxf *.mxf *.nut *.dv *.3gp *.3g2 *.mj2";
    std::string gameMediaFileExt
        =
            "*.roq *.bethsoftvid *.c93 *.dsicin *.dxa *.ea *.cdata *.film_cpk *.idcin *.ipmovie *.psxstr *.rl2 *.siff *.smk *.thp *.tiertexseq *.vmd *.wc3movie *.wsaud *.wsvqa *.txd";
    std::string streamingFileExt = "*.rtsp *.sdp *.nsv *.pva *.msnwctcp *.lmlm4 *.redir";
    std::string animationAndImageFileExt = "*.gif *.flic *.swf *.image2 *.image2pipe";
    std::string rawFileExt
        =
            "*.al *.ul *.s16be *.u16be *.s8 *.u8 *.ub *.uw *.4xm *.MTV *.afc *.aifc *.apc *.apl *.mac *.avs *.302 *.daud *.ffm *.cgi *.mm *.mpegtsraw *.mpegvideo *.nuv *.sw *.sb *.son *.sol *.vfwcap";

    std::string allExt = audioFileExt + " " + videoFileExt + " " + gameMediaFileExt + " " + streamingFileExt + " "
                         + animationAndImageFileExt + " " + rawFileExt;

    std::vector<std::string> filter {
        trc("project", "All supported files") + " (*.mp3,*.aac, ...) (" + allExt + ")",
        trc("project", "Audio files") + " (*.mp3,*.aac, ...) (" + audioFileExt + ")",
        trc("project", "Video files") + " (*.mp4,*.avi, ...) (" + videoFileExt + ")",
        trc("project", "Game media files") + " (*.roq,*.ea, ...) (" + gameMediaFileExt + ")",
        trc("project", "Streaming files") + " (*.rtsp,*.sdp, ...) (" + streamingFileExt + ")",
        trc("project", "Animation and image files") + " (*.gif, ...) (" + animationAndImageFileExt + ")",
        trc("project", "Raw files") + " (*.al,*.ul, ...) (" + rawFileExt + ")"
    };

    io::path_t defaultDir = configuration()->lastOpenedProjectsPath();

    if (defaultDir.empty()) {
        defaultDir = configuration()->userProjectsPath();
    }

    if (defaultDir.empty()) {
        defaultDir = configuration()->defaultUserProjectsPath();
    }

    io::path_t filePath = interactive()->selectOpeningFileSync(muse::trc("project",
                                                                         "Open"), defaultDir, filter, QFileDialog::HideNameFilterDetails);

    if (!filePath.empty()) {
        configuration()->setLastOpenedProjectsPath(io::dirpath(filePath));
    }

    return filePath;
}

IInteractive::Button ProjectActionsController::askAboutSavingProject(IAudacityProjectPtr project)
{
    std::string title = muse::qtrc("project", "Do you want to save changes to the score “%1” before closing?")
                        .arg(project->displayName()).toStdString();

    std::string body = muse::trc("project", "Your changes will be lost if you don’t save them.");

    IInteractive::Result result = interactive()->warningSync(title, body, {
        IInteractive::Button::DontSave,
        IInteractive::Button::Cancel,
        IInteractive::Button::Save
    }, IInteractive::Button::Save);

    return result.standardButton();
}

Ret ProjectActionsController::canSaveProject() const
{
    auto project = currentProject();
    if (!project) {
        LOGW() << "no current project";
        return make_ret(Err::NoProjectError);
    }

    return project->canSave();
}

bool ProjectActionsController::saveProject(SaveMode saveMode, SaveLocationType saveLocationType, bool force)
{
    if (m_isProjectSaving) {
        return false;
    }

    m_isProjectSaving = true;
    DEFER {
        m_isProjectSaving = false;
    };

    IAudacityProjectPtr project = currentProject();

    if (saveMode == SaveMode::Save && !project->isNewlyCreated()) {
        // if (project->isCloudProject()) {
        //     return saveProjectAt(SaveLocation(SaveLocationType::Cloud, project->cloudInfo()));
        // }

        return saveProjectAt(SaveLocation(SaveLocationType::Local));
    }

    //! TODO AU4
    RetVal<SaveLocation> response = openSaveProjectScenario()->askSaveLocation(project, saveMode, saveLocationType);
    if (!response.ret) {
        LOGE() << response.ret.toString();
        return false;
    }

    return saveProjectAt(response.val, saveMode, force);
}

bool ProjectActionsController::saveProjectAt(const SaveLocation& location, SaveMode saveMode, bool force)
{
    //! TODO AU4
    // if (!force) {
    //     Ret ret = canSaveProject();
    //     if (!ret) {
    //         ret = askIfUserAgreesToSaveProjectWithErrors(ret, location);
    //         if (!ret) {
    //             return ret;
    //         }
    //     }
    // }

    if (location.isLocal()) {
        return saveProjectLocally(location.localPath(), saveMode);
    }

    // if (location.isCloud()) {
    //     return saveProjectToCloud(location.cloudInfo(), saveMode);
    // }

    return false;
}

muse::Ret ProjectActionsController::openProject(const muse::io::path_t& givenPath, const String& displayNameOverride)
{
    //! NOTE This method is synchronous,
    //! but inside `multiInstancesProvider` there can be an event loop
    //! to wait for the responses from other instances, accordingly,
    //! the events (like user click) can be executed and this method can be called several times,
    //! before the end of the current call.
    //! So we ignore all subsequent calls until the current one completes.
    if (m_isProjectProcessing) {
        return make_ret(Ret::Code::InternalError);
    }
    m_isProjectProcessing = true;

    DEFER {
        m_isProjectProcessing = false;
    };

    //! Step 1. Take absolute path
    io::path_t actualPath = fileSystem()->absoluteFilePath(givenPath);
    if (actualPath.empty()) {
        // We assume that a valid path has been specified to this method
        return make_ret(Ret::Code::UnknownError);
    }

    //! Step 2. If the project is already open in the current window, then just switch to showing the project
    if (isProjectOpened(actualPath)) {
        return openPageIfNeed(PROJECT_PAGE_URI);
    }

    //! Step 3. Check, if the project already opened in another window, then activate the window with the project
#ifdef MU_BUILD_MULTIINSTANCE_MODULE
    if (multiInstancesProvider()->isProjectAlreadyOpened(actualPath)) {
        multiInstancesProvider()->activateWindowWithProject(actualPath);
        return make_ret(Ret::Code::Ok);
    }
#endif

    //! Step 4. Check, if a any project is already open in the current window,
    //! then create a new instance
    if (globalContext()->currentProject()) {
        QStringList args;
        args << actualPath.toQString();

        if (!displayNameOverride.isEmpty()) {
            args << "--score-display-name-override" << displayNameOverride;
        }
#ifdef MU_BUILD_MULTIINSTANCE_MODULE
        multiInstancesProvider()->openNewAppInstance(args);
        return make_ret(Ret::Code::Ok);
#else
        return muse::make_ret(muse::Ret::Code::NotSupported);
#endif
    }

    //! Step 5. If it's a cloud project, download the latest version
    //! TODO AU4
    // if (configuration()->isCloudProject(actualPath) && !configuration()->isLegacyCloudProject(actualPath)) {
    //     downloadAndOpenCloudProject(configuration()->cloudScoreIdFromPath(actualPath));
    //     return make_ret(Ret::Code::Ok);
    // }

    //! Step 6. Open project in the current window
    return doOpenProject(actualPath);
}

Ret ProjectActionsController::doOpenProject(const io::path_t& filePath)
{
    TRACEFUNC;

    RetVal<IAudacityProjectPtr> rv = loadProject(filePath);
    if (!rv.ret) {
        return rv.ret;
    }

    IAudacityProjectPtr project = rv.val;

    // Check if this is an autosave of a newly created project
    if (!project->isNewlyCreated()) {
        recentFilesController()->prependRecentFile(makeRecentFile(project));
    }

    globalContext()->setCurrentProject(project);

    projectHistory()->init();

    return openPageIfNeed(PROJECT_PAGE_URI);
}

//! TODO AU4
// Ret ProjectActionsController::openAudacityUrl(const QUrl& url)
// {
//
//     if (url.host() == OPEN_PROJECT_URL_HOSTNAME) {
//         return openScoreFromMuseScoreCom(url);
//     }

//     return make_ret(Err::UnsupportedUrl);
// }

RetVal<IAudacityProjectPtr> ProjectActionsController::loadProject(const io::path_t& filePath)
{
    TRACEFUNC;

    //! TODO AU4
    // auto project = projectCreator()->newProject();
    // IF_ASSERT_FAILED(project) {
    //     return make_ret(Ret::Code::InternalError);
    // }
    IAudacityProjectPtr project = std::make_shared<Audacity4Project>();

    //! TODO AU4
    // bool hasUnsavedChanges = project->hasUnsavedChanges();
    // io::path_t loadPath = hasUnsavedChanges ? project->autoSavePath(filePath) : filePath;

    const io::path_t loadPath = filePath;
    const std::string format = io::suffix(filePath);

    if (Ret result = loadWithFallback(project, loadPath, format); !result) {
        return result;
    }

    //! TODO AU4
    // if (hasUnsavedChanges) {
    //     //! NOTE: redirect the project to the original file path
    //     project->setPath(filePath);

    //     project->markAsUnsaved();
    // }

    // Mark project as newly created if it's an autosave of a new project
    if (project->isNewlyCreated()) {
        // Mark as newly created (this will be implemented if needed)
        // project->markAsNewlyCreated();
    }

    return RetVal<IAudacityProjectPtr>::make_ok(project);
}

Ret ProjectActionsController::loadWithFallback(const IAudacityProjectPtr& project,
                                               const muse::io::path_t& loadPath,
                                               const std::string& format)
{
    bool forceLoad = false;
    Ret result = project->load(loadPath, forceLoad, format);

    if (result || result.code() == static_cast<int>(Ret::Code::Cancel)) {
        return result;
    }

    forceLoad = shouldRetryLoadAfterError(result, loadPath);
    if (forceLoad) {
        result = project->load(loadPath, forceLoad, format);
    }

    return result;
}

bool ProjectActionsController::isProjectOpened(const muse::io::path_t& projectPath) const
{
    auto project = globalContext()->currentProject();
    if (!project) {
        return false;
    }

    LOGD() << "project->path: " << project->path() << ", check path: " << projectPath;
    if (project->path() == projectPath) {
        return true;
    }

    return false;
}

RecentFile ProjectActionsController::makeRecentFile(IAudacityProjectPtr project)
{
    RecentFile file;
    file.path = project->path();

    //! TODO AU4
    // if (project->isCloudProject()) {
    //     file.displayNameOverride = project->cloudInfo().name;
    // }

    return file;
}

void ProjectActionsController::clearRecentProjects()
{
    recentFilesController()->clearRecentFiles();
}

bool ProjectActionsController::shouldRetryLoadAfterError(const Ret& ret, const muse::io::path_t& filepath)
{
    if (ret) {
        return true;
    }
    warnProjectCannotBeOpened(ret, filepath);
    return false;
}

void ProjectActionsController::warnProjectCannotBeOpened(const Ret& ret, const muse::io::path_t& filepath) const
{
    const std::string title
        = ret.data<std::string>("title",
                                muse::mtrc("project", "Cannot read file %1")
                                .arg(io::toNativeSeparators(filepath).toString())
                                .toStdString());

    const std::string body
        = ret.data<std::string>("body", !ret.text().empty() ? ret.text() : muse::trc("project",
                                                                                     "An error occurred while reading this file."));
    interactive()->error(title, body);
}

void ProjectActionsController::exportAudio()
{
    interactive()->open(EXPORT_URI);
}

void ProjectActionsController::exportLabels()
{
    NOT_IMPLEMENTED;
}

void ProjectActionsController::exportMIDI()
{
    NOT_IMPLEMENTED;
}

void ProjectActionsController::undo()
{
    NOT_IMPLEMENTED;
}

void ProjectActionsController::redo()
{
    NOT_IMPLEMENTED;
}

muse::Ret ProjectActionsController::openPageIfNeed(muse::Uri pageUri)
{
    if (interactive()->isOpened(pageUri).val) {
        return muse::make_ret(muse::Ret::Code::Ok);
    }

    interactive()->open(pageUri);
    return muse::make_ok();
}

void ProjectActionsController::openCustomFFmpegOptions()
{
    interactive()->open(CUSTOM_FFMPEG_OPTIONS);
}

void ProjectActionsController::openMetadataDialog()
{
    interactive()->open(METADATA_DIALOG_URI);
}
