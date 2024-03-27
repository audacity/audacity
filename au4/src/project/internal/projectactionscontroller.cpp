#include "projectactionscontroller.h"

#include "global/defer.h"
#include "global/translation.h"

#include "audacityproject.h"

#include "log.h"

using namespace mu;
using namespace au::project;

static const mu::Uri PROJECT_PAGE_URI("musescore://project");
static const mu::Uri NEW_PROJECT_URI("musescore://project/new");

void ProjectActionsController::init()
{
    dispatcher()->reg(this, "file-new", this, &ProjectActionsController::newProject);
    dispatcher()->reg(this, "file-open", this, &ProjectActionsController::openProject);
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

    mu::Ret ret = interactive()->open(NEW_PROJECT_URI).ret;

    if (ret) {
        ret = openPageIfNeed(PROJECT_PAGE_URI);
    }

    if (!ret) {
        LOGE() << ret.toString();
    }
}

void ProjectActionsController::openProject(const mu::actions::ActionData& args)
{
    UNUSED(args);
    mu::io::path_t askedPath = selectOpeningFile();

    openProject(askedPath);
}

mu::io::path_t ProjectActionsController::selectOpeningFile()
{
    //! TODO AU4
    std::string allExt = "*.mscz *.mxl *.musicxml *.xml *.mid *.midi *.kar *.md *.mgu *.sgu *.cap *.capx "
                         "*.ove *.scw *.bmw *.bww *.gtp *.gp3 *.gp4 *.gp5 *.gpx *.gp *.ptb *.mei *.mscx *.mscs *.mscz~";

    std::vector<std::string> filter { trc("project", "All supported files") + " (" + allExt + ")",
                                      trc("project", "Audacity files") + " (*.aup3)",
                                      trc("project", "MusicXML files") + " (*.mxl *.musicxml *.xml)",
                                      trc("project", "MIDI files") + " (*.mid *.midi *.kar)",
                                      trc("project", "MuseData files") + " (*.md)",
                                      trc("project", "Capella files") + " (*.cap *.capx)",
                                      trc("project", "BB files (experimental)") + " (*.mgu *.sgu)",
                                      trc("project", "Overture / Score Writer files (experimental)") + " (*.ove *.scw)",
                                      trc("project", "Bagpipe Music Writer files (experimental)") + " (*.bmw *.bww)",
                                      trc("project", "Guitar Pro files") + " (*.gtp *.gp3 *.gp4 *.gp5 *.gpx *.gp)",
                                      trc("project", "Power Tab Editor files (experimental)") + " (*.ptb)",
                                      trc("project", "MEI files") + " (*.mei)",
                                      trc("project", "Uncompressed MuseScore folders (experimental)") + " (*.mscx)",
                                      trc("project", "MuseScore developer files") + " (*.mscs)",
                                      trc("project", "MuseScore backup files") + " (*.mscz~)" };

    io::path_t defaultDir = configuration()->lastOpenedProjectsPath();

    if (defaultDir.empty()) {
        defaultDir = configuration()->userProjectsPath();
    }

    if (defaultDir.empty()) {
        defaultDir = configuration()->defaultUserProjectsPath();
    }

    io::path_t filePath = interactive()->selectOpeningFile(qtrc("project", "Open"), defaultDir, filter);

    if (!filePath.empty()) {
        configuration()->setLastOpenedProjectsPath(io::dirpath(filePath));
    }

    return filePath;
}

mu::Ret ProjectActionsController::openProject(const mu::io::path_t& givenPath, const String& displayNameOverride)
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

    //! Step 2. If the project is already open in the current window, then just switch to showing the notation
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
        return mu::make_ret(mu::Ret::Code::NotSupported);
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

    //! TODO AU4
    // bool isNewlyCreated = projectAutoSaver()->isAutosaveOfNewlyCreatedProject(filePath);
    // if (!isNewlyCreated) {
    //     recentFilesController()->prependRecentFile(makeRecentFile(project));
    // }

    globalContext()->setCurrentProject(project);

    return openPageIfNeed(PROJECT_PAGE_URI);
}

RetVal<IAudacityProjectPtr> ProjectActionsController::loadProject(const io::path_t& filePath)
{
    TRACEFUNC;

    //! TODO AU4
    // auto project = projectCreator()->newProject();
    // IF_ASSERT_FAILED(project) {
    //     return make_ret(Ret::Code::InternalError);
    // }
    IAudacityProjectPtr project = std::make_shared<AudacityProject>();

    //! TODO AU4
    // bool hasUnsavedChanges = projectAutoSaver()->projectHasUnsavedChanges(filePath);
    // io::path_t loadPath = hasUnsavedChanges ? projectAutoSaver()->projectAutoSavePath(filePath) : filePath;
    io::path_t loadPath = filePath;

    std::string format = io::suffix(filePath);

    Ret ret = project->load(loadPath, false /*forceMode*/, format);

    if (!ret) {
        if (ret.code() == static_cast<int>(Ret::Code::Cancel)) {
            return ret;
        }

        //! TODO AU4
        // if (checkCanIgnoreError(ret, loadPath)) {
        //     ret = project->load(loadPath, "" /*stylePath*/, true /*forceMode*/, format);
        // }

        if (!ret) {
            return ret;
        }
    }

    //! TODO AU4
    // if (hasUnsavedChanges) {
    //     //! NOTE: redirect the project to the original file path
    //     project->setPath(filePath);

    //     project->markAsUnsaved();
    // }

    //! TODO AU4
    // bool isNewlyCreated = projectAutoSaver()->isAutosaveOfNewlyCreatedProject(filePath);
    // if (isNewlyCreated) {
    //     project->markAsNewlyCreated();
    // }

    return RetVal<IAudacityProjectPtr>::make_ok(project);
}

bool ProjectActionsController::isProjectOpened(const mu::io::path_t& projectPath) const
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

mu::Ret ProjectActionsController::openPageIfNeed(mu::Uri pageUri)
{
    if (interactive()->isOpened(pageUri).val) {
        return mu::make_ret(mu::Ret::Code::Ok);
    }

    return interactive()->open(pageUri).ret;
}
