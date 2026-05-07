#include "projectactionscontroller.h"

#include <QFileDialog>

#include "au3cloud/internal/au3audiocomservice.h"
#include "framework/global/async/async.h"
#include "framework/global/defer.h"
#include "framework/global/translation.h"
#include "framework/global/io/path.h"
#include "framework/global/io/fileinfo.h"
#include "framework/global/progress.h"
#include "framework/global/log.h"
#include "framework/global/types/ret.h"
#include "framework/ui/view/iconcodes.h"
#include "framework/interactive/iinteractive.h"

#include "au3cloud/au3clouderrors.h"

#include "audacityproject.h"
#include "projecterrors.h"
#include "project/types/projecttypes.h"

using namespace muse;
using namespace au::project;

static const muse::Uri PROJECT_PAGE_URI("audacity://project");
static const muse::Uri HOME_PAGE_URI("audacity://home");
static const muse::Uri NEW_PROJECT_URI("audacity://project/new");

static const muse::Uri SAVE_TO_CLOUD_URI("audacity://project/savetocloud");
static const muse::Uri EXPORT_URI("audacity://project/export");
static const muse::Uri CUSTOM_FFMPEG_OPTIONS("audacity://project/export/ffmpeg");
static const muse::Uri METADATA_DIALOG_URI("audacity://project/export/metadata");
static const muse::Uri EXPORT_LABELS_URI("audacity://project/export/labels");
static const muse::Uri CUSTOM_MAPPING("audacity://project/export/mapping");

static const QString AUDACITY_URL_SCHEME("audacity");
static const QString OPEN_PROJECT_URL_HOSTNAME("open-project");

static const muse::actions::ActionCode OPEN_CUSTOM_FFMPEG_OPTIONS("open-custom-ffmpeg-options");
static const muse::actions::ActionCode OPEN_METADATA_DIALOG("open-metadata-dialog");
static const muse::actions::ActionCode OPEN_CUSTOM_MAPPING("open-custom-mapping");
static const muse::actions::ActionQuery OPEN_CLOUD_AUDIO_FILE_URI("audacity://cloud/open-audio-file");

ProjectActionsController::ProjectActionsController(muse::modularity::ContextPtr ctx)
    : muse::Contextable(ctx)
{
}

void ProjectActionsController::init()
{
    dispatcher()->reg(this, "file-new", this, &ProjectActionsController::newProject);
    dispatcher()->reg(this, "file-open", this, &ProjectActionsController::open);
    dispatcher()->reg(this, "cloud-file-open", this, &ProjectActionsController::openCloudProject);
    dispatcher()->reg(this, "clear-recent", this, &ProjectActionsController::clearRecentProjects);
    dispatcher()->reg(this, "project-import", this, &ProjectActionsController::importFiles);
    dispatcher()->reg(this, "project-import-startup-media", this, &ProjectActionsController::importStartupMedia);

    dispatcher()->reg(this, "file-save", [this]() { saveProject(SaveMode::Save); });
    //! TODO AU4: decide whether to implement these functions from scratch in AU4 or
    //! to install our own implementation of the UI (BasicUI API)
    //! right now there's only BasicUI stub which means there's no progress dialog shown on saving
    dispatcher()->reg(this, "file-save-as", [this]() { saveProject(SaveMode::SaveAs); });
    dispatcher()->reg(this, "file-save-backup", [this]() { saveProject(SaveMode::SaveCopy); });

    dispatcher()->reg(this, "file-share-audio", this, &ProjectActionsController::shareAudio);
    dispatcher()->reg(this, OPEN_CLOUD_AUDIO_FILE_URI, this, &ProjectActionsController::openCloudAudioFile);

    dispatcher()->reg(this, "export-audio", this, &ProjectActionsController::exportAudio);
    dispatcher()->reg(this, "export-labels", this, &ProjectActionsController::exportLabels);
    dispatcher()->reg(this, "export-midi", this, &ProjectActionsController::exportMIDI);

    dispatcher()->reg(this, "manage-metadata", this, &ProjectActionsController::openMetadataDialog);

    dispatcher()->reg(this, "file-close", [this]() {
        // reset preferred export sample rate
        exportConfiguration()->setExportSampleRate(-1);

        if (multiwindowsProvider()->windowCount() > 1) {
            mainWindow()->qWindow()->close();
            return;
        }

        closeOpenedProject(false);
    });

    dispatcher()->reg(this, OPEN_CUSTOM_FFMPEG_OPTIONS, this, &ProjectActionsController::openCustomFFmpegOptions);
    dispatcher()->reg(this, OPEN_METADATA_DIALOG, this, &ProjectActionsController::openMetadataDialog);
    dispatcher()->reg(this, OPEN_CUSTOM_MAPPING, this, &ProjectActionsController::openCustomMapping);
}

const muse::actions::ActionCodeList& ProjectActionsController::prohibitedActionsWhileRecording() const
{
    static const std::vector<muse::actions::ActionCode> PROHIBITED_WHILE_RECORDING {
        "file-new",
        "file-open",
        "cloud-file-open",
        "audacity://cloud/open-audio-file",
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
            "project-import-startup-media",
            "cloud-file-open",
            "continue-last-session",
            "clear-recent",
            "audacity://cloud/open-audio-file",
            "plugin-manager",
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
        muse::io::paths_t filenames = file.isNull() ? selectOpeningFiles() : muse::io::paths_t { file.path() };
        muse::io::path_t filename = filenames.empty() ? muse::io::path_t() : filenames.front();

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

        return openProject(filename, file.displayNameOverride, file.cloudProjectId);
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
    //! but inside `multiwindowsProvider` there can be an event loop
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
        //! Check, if any project is already open in the current window
        //! and there is already a created instance without a project, then activate it
        if (multiwindowsProvider()->isHasWindowWithoutProject()) {
            multiwindowsProvider()->activateWindowWithoutProject();
            return;
        }

        //! Otherwise, we will create a new instance
        QStringList args;
        args << "--session-type" << "start-with-new";
        multiwindowsProvider()->openNewWindow(args);
        return;
    }

    auto project = createProjectInCurrentWindow();
    if (!project) {
        LOGE() << "Failed to create new project in current window";
    }

    muse::async::Async::call(this, [this, ok = !!project]() {
        openPageIfNeed(ok ? PROJECT_PAGE_URI : HOME_PAGE_URI);
    });
}

void ProjectActionsController::open(const muse::actions::ActionData& args)
{
    const QUrl url = !args.empty() ? args.arg<QUrl>(0) : QUrl();
    const QString displayNameOverride = args.count() >= 2 ? args.arg<QString>(1) : QString();
    const muse::io::paths_t filePaths = url.isLocalFile() ? muse::io::paths_t { muse::io::path_t(url) } : selectOpeningFiles();

    Ret ret = make_ret(Ret::Code::Cancel);

    if (url.isValid() && !url.isEmpty() && !url.isLocalFile()) {
        ret = openProject(ProjectFile(url, displayNameOverride));
    } else if (filePaths.empty()) {
        ret = make_ret(Ret::Code::Cancel);
    } else if (filePaths.size() > 1) {
        auto projectIt = std::find_if(filePaths.cbegin(), filePaths.cend(), [](const auto& filePath) {
            return au::project::isAudacityFile(filePath);
        });

        if (projectIt != filePaths.cend()) {
            ret = openProject(ProjectFile(QUrl::fromLocalFile(projectIt->toQString()), displayNameOverride));
        } else {
            muse::io::paths_t supportedFilePaths;
            supportedFilePaths.reserve(filePaths.size());
            for (const auto& filePath : filePaths) {
                if (isFileSupported(filePath)) {
                    supportedFilePaths.emplace_back(filePath);
                }
            }

            if (supportedFilePaths.empty()) {
                ret = make_ret(Err::UnsupportedUrl);
            } else {
                ret = processMediaFiles(supportedFilePaths);
            }
        }
    } else if (!isFileSupported(filePaths.front())) {
        interactive()->error(muse::trc("project", "Error opening file"),
                             muse::mtrc("project", "Could not open file: %1").arg(filePaths.front().toString()).toStdString());
        ret = make_ret(Err::UnsupportedUrl);
    } else if (au::project::isAudacityFile(filePaths.front())) {
        ret = openProject(ProjectFile(QUrl::fromLocalFile(filePaths.front().toQString()), displayNameOverride));
    } else {
        ret = processMediaFiles({ filePaths.front() });
    }

    if (!ret) {
        openPageIfNeed(HOME_PAGE_URI);
    }
}

void ProjectActionsController::openCloudProject(const muse::actions::ActionData& args)
{
    if (args.count() != 3) {
        return;
    }

    const QString cloudProjectId = args.arg<QString>(0);
    const QUrl url = args.arg<QUrl>(1);
    const QString displayName = args.arg<QString>(2);

    Ret ret = openProject(muse::io::path_t(url), displayName, cloudProjectId);
    if (!ret) {
        openPageIfNeed(HOME_PAGE_URI);
    }
}

void ProjectActionsController::importFiles(const muse::actions::ActionData& args)
{
    const IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return;
    }

    muse::io::paths_t filePaths;
    if (!args.empty()) {
        const QStringList files = args.arg<QStringList>(0);
        filePaths.reserve(files.size());
        for (const QString& file : files) {
            const io::path_t path(file);
            const io::path_t actualPath = fileSystem()->absoluteFilePath(path);
            filePaths.emplace_back(actualPath.empty() ? path : actualPath);
        }
    } else {
        filePaths = selectImportFiles();
    }

    if (filePaths.empty()) {
        return;
    }

    project->import(filePaths);
}

void ProjectActionsController::importStartupMedia(const muse::actions::ActionData& args)
{
    const QStringList files = !args.empty() ? args.arg<QStringList>(0) : QStringList();
    const bool removeAfterImport = args.count() >= 2 ? args.arg<bool>(1) : false;

    muse::io::paths_t filePaths;
    filePaths.reserve(files.size());
    for (const QString& file : files) {
        filePaths.emplace_back(file);
    }

    Ret ret = processMediaFiles(filePaths);
    if (removeAfterImport) {
        for (const auto& filePath : filePaths) {
            fileSystem()->remove(filePath);
        }
    }

    if (!ret) {
        openPageIfNeed(HOME_PAGE_URI);
    }
}

muse::Ret ProjectActionsController::processMediaFiles(const muse::io::paths_t& paths)
{
    if (paths.empty()) {
        return make_ret(Ret::Code::Cancel);
    }

    muse::io::paths_t actualPaths;
    actualPaths.reserve(paths.size());
    for (const auto& givenPath : paths) {
        io::path_t actualPath = fileSystem()->absoluteFilePath(givenPath);
        if (actualPath.empty()) {
            return make_ret(Ret::Code::UnknownError);
        }

        actualPaths.emplace_back(actualPath);
    }

    if (globalContext()->currentProject()) {
        QStringList args;
        args << "--session-type" << "start-with-new";
        for (const auto& actualPath : actualPaths) {
            args << "--import-media-file" << actualPath.toQString();
        }

        multiwindowsProvider()->openNewWindow(args);
        return make_ret(Ret::Code::Ok);
    }

    IAudacityProjectPtr project = createProjectInCurrentWindow();
    if (!project) {
        return make_ret(Ret::Code::InternalError);
    }

    Ret ret = openPageIfNeed(PROJECT_PAGE_URI);
    if (!ret) {
        return ret;
    }

    return project->import(actualPaths);
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

    const std::string ext = io::suffix(path);
    if (ext.empty()) {
        return false;
    }

    const auto supportedExtensions = importer()->supportedExtensions();
    return std::find(supportedExtensions.cbegin(), supportedExtensions.cend(), ext) != supportedExtensions.cend();
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
        interactive()->closeAllDialogsSync();

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

bool ProjectActionsController::saveProjectToCloud(const CloudProjectInfo& cloudInfo, SaveMode saveMode, bool forceOverwrite)
{
    if (!audioComService()->enabled()) {
        LOGE() << "Cloud support is not available";
        return false;
    }

    if (!ensureAuthorization()) {
        return false;
    }

    io::path_t cloudProjectsPath = configuration()->cloudProjectsPath();
    if (cloudProjectsPath.empty()) {
        LOGE() << "Cloud projects path is not set";
        return false;
    }

    io::path_t projectFilePath = cloudProjectsPath.appendingComponent(cloudInfo.name).appendingSuffix(au::project::AUP4);

    IAudacityProjectPtr project = currentProject();
    if (!project) {
        LOGE() << "No project opened";
        return false;
    }

    auto [uploadRet, progress] = audioComService()->uploadProject(project, cloudInfo.name.toStdString(), [this, projectFilePath]() {
        return saveProjectLocally(projectFilePath, SaveMode::Save);
    }, forceOverwrite);

    if (!uploadRet) {
        handleCloudSaveError(uploadRet);
        return false;
    }

    if (!progress) {
        LOGE() << "Failed to start cloud upload";
        return false;
    }

    progress->finished().onReceive(this, [this, projectFilePath](const ProgressResult& result) {
        if (!result.ret.success()) {
            handleCloudSaveError(result.ret);
            return;
        }

        const bool dismissable = false;
        toastService()->show(trc("global", "Success"),
                             trc("project",
                                 "All saved changes will now update to the cloud.\nYou can manage this file from your updated projects page on audio.com"),
                             muse::ui::IconCode::Code::TICK,
                             dismissable,
        {
            { trc("project", "Dismiss"), au::toast::ToastActionCode::None },
            { trc("cloud", "View on audio.com"), au::toast::ToastActionCode::Custom }
        }
                             ).onResolve(this, [this, url = result.val.toQString()](au::toast::ToastActionCode actionCode) {
            if (actionCode == au::toast::ToastActionCode::Custom) {
                platformInteractive()->openUrl(url);
            }
        });
    });

    const bool dismissible = false;
    const bool showProgressInfo = true;
    toastService()->showWithProgress(
        trc("project", "Upload project to audio.com…"),
        {},
        progress,
        muse::ui::IconCode::Code::CLOUD,
        dismissible,
        {},
        showProgressInfo
        );

    return true;
}

bool ProjectActionsController::saveProjectLocally(const muse::io::path_t& filePath, SaveMode saveMode)
{
    IAudacityProjectPtr project = currentProject();
    if (!project) {
        return false;
    }

    Ret ret = project->save(filePath, saveMode);
    if (!ret) {
        LOGE() << ret.toString();
        return false;
    }

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

muse::io::paths_t ProjectActionsController::selectOpeningFiles()
{
    std::vector<std::string> supportedExtensions = importer()->supportedExtensions();

    std::string mediaExt;
    for (const std::string& ext : supportedExtensions) {
        if (ext.empty()) {
            continue;
        }

        if (!mediaExt.empty()) {
            mediaExt += " ";
        }

        mediaExt += "*." + ext;
    }

    const std::string projectExt = "*.aup3 *.aup4";
    const std::string allExt = mediaExt.empty() ? projectExt : projectExt + " " + mediaExt;

    std::vector<std::string> filter {
        trc("project", "All supported files") + " (*.aup4,*.mp3, ...) (" + allExt + ")",
        trc("project", "Audacity project files") + " (*.aup3,*.aup4, ...) (" + projectExt + ")",
        trc("project", "Audacity 3 files") + " (*.aup3, ...) (*.aup3)",
        trc("project", "Audacity 4 files") + " (*.aup4, ...) (*.aup4)",
        trc("project", "Importable audio and media files") + " (*.mp3,*.aac, ...) (" + mediaExt + ")",
    };

    io::path_t defaultDir = configuration()->lastOpenedProjectsPath();

    if (defaultDir.empty()) {
        defaultDir = configuration()->userProjectsPath();
    }

    if (defaultDir.empty()) {
        defaultDir = configuration()->defaultUserProjectsPath();
    }

    io::paths_t filePaths = interactive()->selectOpeningFilesSync(muse::trc("project",
                                                                            "Open"), defaultDir, filter,
                                                                  QFileDialog::HideNameFilterDetails);

    if (!filePaths.empty()) {
        configuration()->setLastOpenedProjectsPath(io::dirpath(filePaths.front()));
    }

    return filePaths;
}

muse::io::paths_t ProjectActionsController::selectImportFiles()
{
    std::string audioFileExt
        = "*.aac *.ac3 *.mp2 *.mp3 *.wma *.wav *.flac *.ogg *.opus *.aif *.aiff *.amr *.ape *.au *.dts *.mpc *.tta *.wv *.shn *.voc *.mmf";
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

    io::paths_t filePaths = interactive()->selectOpeningFilesSync(muse::trc("project",
                                                                            "Open"), defaultDir, filter,
                                                                  QFileDialog::HideNameFilterDetails);

    if (!filePaths.empty()) {
        configuration()->setLastOpenedProjectsPath(io::dirpath(filePaths.front()));
    }

    return filePaths;
}

IInteractive::Button ProjectActionsController::askAboutSavingProject(IAudacityProjectPtr project)
{
    std::string title;

    if (project->isNewlyCreated()) {
        title = muse::qtrc("project", "Do you want to save changes to the project before closing?").toStdString();
    } else {
        title = muse::qtrc("project", "Do you want to save changes to the project “%1” before closing?")
                .arg(project->displayName()).toStdString();
    }

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
        if (project->isCloudProject()) {
            return saveProjectAt(SaveLocation(SaveLocationType::Cloud, CloudProjectInfo { QUrl {}, {}, project->displayName() }));
        }

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

    if (location.isCloud()) {
        return saveProjectToCloud(location.cloudInfo(), saveMode);
    }

    return false;
}

muse::Ret ProjectActionsController::openProject(const muse::io::path_t& path, const String& displayNameOverride,
                                                const String& projectId)
{
    //! NOTE This method is synchronous,
    //! but inside `multiwindowsProvider` there can be an event loop
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
    io::path_t actualPath = fileSystem()->absoluteFilePath(path);
    if (actualPath.empty()) {
        // We assume that a valid path has been specified to this method
        return make_ret(Ret::Code::UnknownError);
    }

    //! Step 2. If the project is already open in the current window, then just switch to showing the project
    if (isProjectOpened(actualPath)) {
        return openPageIfNeed(PROJECT_PAGE_URI);
    }

    //! Step 3. Check, if the project already opened in another window, then activate the window with the project
    if (multiwindowsProvider()->isProjectAlreadyOpened(actualPath)) {
        multiwindowsProvider()->activateWindowWithProject(actualPath);
        return make_ret(Ret::Code::Ok);
    }

    //! Step 4. Check, if a any project is already open in the current window,
    //! then create a new instance
    if (globalContext()->currentProject()) {
        QStringList args;
        args << actualPath.toQString();

        if (!displayNameOverride.isEmpty()) {
            args << "--project-display-name-override" << displayNameOverride;
        }
        if (!projectId.empty()) {
            args << "--cloud-project-id" << projectId;
        }
        multiwindowsProvider()->openNewWindow(args);
        return make_ret(Ret::Code::Ok);
    }

    //! Step 5. If it's a cloud project, download the latest version
    if (configuration()->isCloudProject(actualPath)) {
        return openCloudProject(actualPath, projectId);
    }

    //! Step 6. Open project in the current window
    return doOpenProject(actualPath);
}

IAudacityProjectPtr ProjectActionsController::createProjectInCurrentWindow()
{
    IAudacityProjectPtr project = std::make_shared<Audacity4Project>(iocContext());
    Ret ret = project->createNew();
    if (!ret) {
        LOGE() << ret.toString();
        return nullptr;
    }

    globalContext()->setCurrentProject(project);
    projectHistory()->init();

    return project;
}

Ret ProjectActionsController::openCloudProject(const io::path_t& localPath, const String& projectId, bool forceOverwrite)
{
    if (!audioComService()->enabled()) {
        LOGE() << "Cloud support is not available";
        return make_ret(Ret::Code::NotSupported);
    }

    if (!ensureAuthorization()) {
        return make_ret(Ret::Code::Cancel);
    }

    const std::string cloudProjectIdStr = projectId.toStdString();
    auto [openRet, progress] = audioComService()->openCloudProject(localPath, cloudProjectIdStr, forceOverwrite);
    if (!openRet) {
        handleCloudOpenError(openRet, localPath, cloudProjectIdStr);
        return openRet;
    }

    if (!progress) {
        return make_ret(Ret::Code::UnknownError);
    }

    progress->finished().onReceive(this, [this, localPath, cloudProjectIdStr](const ProgressResult& result) {
        if (!result.ret) {
            handleCloudOpenError(result.ret, localPath, cloudProjectIdStr);
            return;
        }

        doOpenProject(localPath);

        auto project = globalContext()->currentProject();
        if (!project) {
            return;
        }

        if (!ensureAuthorization()) {
            return;
        }

        auto [syncRet, syncProgress] = audioComService()->resumeProjectSync(project);
        if (!syncRet || !syncProgress || syncProgress->isCanceled()) {
            return;
        }

        syncProgress->finished().onReceive(this, [this](const ProgressResult& result) {
            if (!result.ret.success()) {
                handleCloudSaveError(result.ret);
            }
        });

        const bool dismissible = false;
        const bool showProgressInfo = true;
        toastService()->showWithProgress(
            trc("project", "Resuming sync to audio.com…"),
            {},
            syncProgress,
            muse::ui::IconCode::Code::CLOUD,
            dismissible,
            {},
            showProgressInfo
            );
    });

    interactive()->showProgress(trc("project", "Syncing project from cloud…"), *progress);

    return make_ret(Ret::Code::Ok);
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

#ifndef AU_LOAD_TIMETRACK
    const auto trackeditProject = project->trackeditProject();
    if (trackeditProject && trackeditProject->timeTrackFound()) {
        interactive()->infoSync(muse::trc("project/open", "Time Track not supported"),
                                muse::trc("project/open",
                                          "The project contains a time track, which is not yet supported in Audacity 4, and will need to be removed. This does not affect your original Audacity 3 project."),
        {
            muse::IInteractive::ButtonData(
                muse::IInteractive::Button::Ok, muse::trc("project/open", "OK"), false)
        });

        // When saving we do a full project rewrite
        // We need to save the project immediately to remove the time track from the project file and avoid showing this message repeatedly
        saveProject(SaveMode::Save);
    }
#endif

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
    IAudacityProjectPtr project = std::make_shared<Audacity4Project>(iocContext());

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

void ProjectActionsController::shareAudio()
{
    if (!audioComService()->enabled()) {
        LOGE() << "Cloud support is not available";
        return;
    }

    muse::UriQuery query(SAVE_TO_CLOUD_URI);
    query.addParam("formTitle", Val(trc("cloud", "Track title")));
    query.addParam("title", Val(trc("cloud", "Share audio")));
    query.addParam("actionText", Val(trc("cloud", "Share")));

    RetVal<Val> rv = interactive()->openSync(query);
    if (!rv.ret) {
        return;
    }

    std::string title = rv.val.toQString().toStdString();
    if (title.empty()) {
        return;
    }

    auto [shareRet, progress] = audioComService()->shareAudio(title);
    if (!shareRet || !progress) {
        return;
    }

    progress->finished().onReceive(this, [this](const ProgressResult& result) {
        if (result.ret.success()) {
            const bool dismissable = false;
            toastService()->show(trc("global", "Success"),
                                 trc("cloud", "Audio shared to audio.com"),
                                 muse::ui::IconCode::Code::TICK,
                                 dismissable,
            {
                { trc("global", "Dismiss"), au::toast::ToastActionCode::None },
                { trc("cloud", "View on audio.com"), au::toast::ToastActionCode::Custom }
            }
                                 ).onResolve(this, [this, url = result.val.toQString()](au::toast::ToastActionCode actionCode) {
                if (actionCode == au::toast::ToastActionCode::Custom) {
                    platformInteractive()->openUrl(url);
                }
            });
        } else {
            handleCloudSaveError(result.ret);
        }
    });

    const bool dismissable = false;
    const bool showProgressInfo = true;
    toastService()->showWithProgress(
        trc("cloud", "Sharing audio to audio.com…"),
        {},
        progress,
        muse::ui::IconCode::Code::SHARE_AUDIO,
        dismissable,
        {},
        showProgressInfo
        );
}

void ProjectActionsController::openCloudAudioFile(const muse::actions::ActionQuery& query)
{
    const auto audioId = query.param("audioId").toString();
    if (audioId.empty()) {
        return;
    }

    auto [downloadRet, progress] = audioComService()->downloadAudioFile(audioId);
    if (!downloadRet) {
        handleCloudAudioOpenError(downloadRet);
        return;
    }

    if (!progress) {
        return;
    }

    progress->finished().onReceive(this, [this](const ProgressResult& result) {
        if (!result.ret) {
            handleCloudAudioOpenError(result.ret);
            return;
        }

        const auto localPath = result.val.toQString();
        const auto project = globalContext()->currentProject();
        if (project) {
            QStringList args;
            args << "--session-type" << "start-with-new";
            args << "--import-media-file" << localPath;
            args << "--remove-media-after-import";
            multiwindowsProvider()->openNewWindow(args);
            return;
        }

        auto newproject = createProjectInCurrentWindow();
        if (!newproject) {
            return;
        }

        const auto importRet = newproject->import(muse::io::paths_t { localPath });
        fileSystem()->remove(localPath);
        if (!importRet) {
            LOGE() << importRet.toString();
            return;
        }

        openPageIfNeed(PROJECT_PAGE_URI);
    });

    interactive()->showProgress(muse::trc("cloud", "Downloading audio from cloud…"), *progress);
}

void ProjectActionsController::exportAudio()
{
    interactive()->open(EXPORT_URI);
}

void ProjectActionsController::exportLabels(const actions::ActionData& args)
{
    muse::UriQuery query(EXPORT_LABELS_URI);

    trackedit::TrackId trackId = args.count() == 1 ? args.arg<trackedit::LabelKey>(0).trackId : -1;
    query.addParam("trackId", Val(trackId));

    interactive()->open(query);
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

void ProjectActionsController::openCustomMapping()
{
    interactive()->open(CUSTOM_MAPPING);
}

muse::Ret ProjectActionsController::ensureAuthorization()
{
    if (authorization()->isAuthorized()) {
        return make_ret(Ret::Code::Ok);
    }

    muse::actions::ActionQuery query("audacity://cloud/open-signin-dialog");
    query.addParam("sync", muse::Val(true));
    query.addParam("showTourPage", muse::Val(false));

    dispatcher()->dispatch(query);

    return authorization()->isAuthorized() ? make_ret(Ret::Code::Ok) : make_ret(Ret::Code::Cancel);
}

void ProjectActionsController::handleCloudOpenError(const muse::Ret& error, const io::path_t& localPath,
                                                    const std::string& cloudProjectId)
{
    using Err = au::au3cloud::Err;
    const auto err = static_cast<Err>(error.code());

    if (err == Err::SyncResultNotFound && fileSystem()->exists(localPath)) {
        doOpenProject(localPath);
    }

    const auto ret = openSaveProjectScenario()->showCloudOpenError(error, localPath);

    switch (ret.code()) {
    case IOpenSaveProjectScenario::RET_CODE_OPEN_LOCAL:
        doOpenProject(localPath);
        break;
    case IOpenSaveProjectScenario::RET_CODE_SAVE_LOCALLY_AND_REMOVE_CACHE: {
        IAudacityProjectPtr project = currentProject();
        if (!project) {
            break;
        }
        const auto askRet = openSaveProjectScenario()->askLocalPath(project, SaveMode::Save);
        if (!askRet.ret || askRet.val.empty()) {
            break;
        }
        saveProjectLocally(askRet.val, SaveMode::Save);
        fileSystem()->remove(localPath);
        break;
    }
    case IOpenSaveProjectScenario::RET_CODE_SAVE_TO_CLOUD: {
        IAudacityProjectPtr project = currentProject();
        if (!project) {
            break;
        }
        saveProjectToCloud(CloudProjectInfo { QUrl {}, {}, project->displayName() }, SaveMode::Save);
        break;
    }
    case IOpenSaveProjectScenario::RET_CODE_OPEN_CLOUD_FORCE:
        openCloudProject(localPath, {}, true);
        break;
    case IOpenSaveProjectScenario::RET_CODE_LOAD_LATEST_SYNCED:
        openCloudProject(localPath, muse::String::fromStdString(cloudProjectId), true);
        break;
    case IOpenSaveProjectScenario::RET_CODE_OPEN_ON_AUDIOCOM:
        if (!cloudProjectId.empty()) {
            platformInteractive()->openUrl(audioComService()->getCloudProjectPage(cloudProjectId));
        }
        break;
    default:
        break;
    }
}

void ProjectActionsController::handleCloudSaveError(const muse::Ret& error)
{
    IAudacityProjectPtr project = currentProject();
    if (!project) {
        return;
    }

    const auto ret = openSaveProjectScenario()->showCloudSaveError(error);

    switch (ret.code()) {
    case IOpenSaveProjectScenario::RET_CODE_SAVE_LOCALLY: {
        const auto askRet = openSaveProjectScenario()->askLocalPath(project, SaveMode::Save);
        if (!askRet.ret || askRet.val.empty()) {
            break;
        }
        saveProjectLocally(askRet.val, SaveMode::Save);
        break;
    }
    case IOpenSaveProjectScenario::RET_CODE_SAVE_LOCALLY_AND_REMOVE_CACHE: {
        const auto oldPath = project->path();
        const auto askRet = openSaveProjectScenario()->askLocalPath(project, SaveMode::Save);
        if (!askRet.ret || askRet.val.empty()) {
            break;
        }
        saveProjectLocally(askRet.val, SaveMode::Save);
        fileSystem()->remove(oldPath);
        break;
    }
    case IOpenSaveProjectScenario::RET_CODE_SAVE_TO_CLOUD:
        saveProjectToCloud(CloudProjectInfo { QUrl {}, {}, project->displayName() }, SaveMode::Save);
        break;
    case IOpenSaveProjectScenario::RET_CODE_SAVE_TO_CLOUD_FORCE:
        saveProjectToCloud(CloudProjectInfo { QUrl {}, {}, project->displayName() }, SaveMode::Save, true);
        break;
    case IOpenSaveProjectScenario::RET_CODE_CLOSE_AND_OPEN_CLOUD_FORCE: {
        const io::path_t localPath = project->path();
        closeOpenedProject(false);
        openCloudProject(localPath, {}, true);
        break;
    }
    default:
        break;
    }
}

void ProjectActionsController::handleCloudAudioOpenError(const muse::Ret& error)
{
    openSaveProjectScenario()->showCloudAudioOpenError(error);
}
