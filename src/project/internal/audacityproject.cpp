#include "audacityproject.h"

#include "au3wrap/iau3project.h"
#include "iprojectautosaver.h"
#include "projecterrors.h"
#include "global/io/ioretcodes.h"

#include "log.h"

using namespace muse;
using namespace au::project;
using namespace au::trackedit;
using namespace au::projectscene;

static QString projectDefaultTitle()
{
    return muse::qtrc("project", "Untitled project");
}

Audacity4Project::Audacity4Project()
{
}

Ret Audacity4Project::createNew()
{
    m_au3Project = au3ProjectCreator()->create();
    m_au3Project->open();
    m_trackeditProject = trackeditProjectCreator()->create(m_au3Project);
    m_isNewlyCreated = true;
    m_viewState = viewStateCreator()->createViewState();

    return muse::make_ret(Ret::Code::Ok);
}

muse::Ret Audacity4Project::load(const muse::io::path_t& path, bool forceMode, const std::string& format_)
{
    TRACEFUNC;

    std::string format = format_.empty() ? io::suffix(path) : format_;

    LOGD() << "try load: " << path << ", format: " << format;

    //setupProject();
    setPath(path);

    //! TODO AU4
    // if (!isAudacityFile(format)) {
    //     Ret ret = doImport(path, forceMode);
    //     if (ret) {
    //         listenIfNeedSaveChanges();
    //     }

    //     return ret;
    // }

    Ret ret = doLoad(path, forceMode, format);
    if (!ret) {
        LOGE() << "failed load, err: " << ret.toString();
        return ret;
    }

    //! TODO AU4
    // listenIfNeedSaveChanges();

    return ret;
}

muse::Ret Audacity4Project::doLoad(const io::path_t& path, bool forceMode, const std::string& format)
{
    TRACEFUNC;

    UNUSED(forceMode);
    UNUSED(format);

    m_au3Project = au3ProjectCreator()->create();
    bool isLoaded = m_au3Project->load(path);
    if (!isLoaded) {
        LOGE() << "Failed load:" << path;
        return muse::make_ret(muse::Ret::Code::UnknownError);
    }

    LOGI() << "success loaded au3 project: " << m_au3Project->title();

    m_trackeditProject = trackeditProjectCreator()->create(m_au3Project);

    //! NOTE At the moment, view state don't saved and loaded
    m_viewState = viewStateCreator()->createViewState();

    return muse::make_ret(Ret::Code::Ok);
}

void Audacity4Project::close()
{
    m_aboutCloseBegin.notify();

    const auto history = projectHistory();
    history->undoUnsaved();
    history->clearUnsaved();
    // Do not save a project that has never explicitly been saved.
    if (m_au3Project->hasSavedVersion()) {
        //! Important!
        //! If some unsaved changes were made and we don't save the project after having cleared the history,
        //! the project will be corrupted and can't be opened again.
        save();
    }

    clipboard()->clearTrackData();
    m_au3Project->close();

    m_aboutCloseEnd.notify();
}

muse::async::Notification Audacity4Project::aboutCloseBegin() const
{
    return m_aboutCloseBegin;
}

muse::async::Notification Audacity4Project::aboutCloseEnd() const
{
    return m_aboutCloseEnd;
}

QString Audacity4Project::displayName() const
{
    if (isNewlyCreated()) {
        if (m_path.empty()) {
            QString workTitle = QString::fromStdString(m_au3Project->title());
            if (workTitle.isEmpty()) {
                return projectDefaultTitle();
            }
            return workTitle;
        }
        return io::filename(m_path).toQString();
    }

    //! TODO AU4
    // if (isCloudProject()) {
    //     return m_cloudInfo.name;
    // }

    return io::filename(m_path, false /*isSuffixInteresting*/).toQString();
}

muse::async::Notification Audacity4Project::displayNameChanged() const
{
    return m_displayNameChanged;
}

void Audacity4Project::setPath(const io::path_t& path)
{
    if (m_path == path) {
        return;
    }

    m_path = path;
    m_pathChanged.notify();
}

bool Audacity4Project::isNewlyCreated() const
{
    return m_isNewlyCreated;
}

bool Audacity4Project::isImported() const
{
    return m_isImported;
}

ValNt<bool> Audacity4Project::needSave() const
{
    //! TODO AU4
    // const mu::engraving::MasterScore* score = m_masterNotation->masterScore();

    // ValNt<bool> needSave;
    // needSave.val = score && !score->saved();
    // needSave.notification = m_needSaveNotification;

    // return needSave;
    return muse::ValNt<bool>();
}

Ret Audacity4Project::canSave() const
{
    //! TODO AU4
    // if (!m_masterNotation->hasParts()) {
    //     return make_ret(Err::NoPartsError);
    // }

    // Ret ret = m_engravingProject->checkCorrupted();
    // if (!ret) {
    //     Err errorCode = m_engravingProject->isCorruptedUponLoading() ? Err::CorruptionUponOpenningError : Err::CorruptionError;
    //     ret.setCode(static_cast<int>(errorCode));
    // }

    // return ret;
    return true;
}

bool Audacity4Project::needAutoSave() const
{
    return m_needAutoSave;
}

void Audacity4Project::setNeedAutoSave(bool val)
{
    m_needAutoSave = val;
}

Ret Audacity4Project::save(const muse::io::path_t& path, SaveMode saveMode)
{
    TRACEFUNC;

    switch (saveMode) {
    // case SaveMode::SaveSelection:
    //     return saveSelectionOnScore(path);
    case SaveMode::Save:
    case SaveMode::SaveAs:
    case SaveMode::SaveCopy: {
        muse::io::path_t savePath = path;
        if (savePath.empty()) {
            IF_ASSERT_FAILED(!m_path.empty()) {
                return false;
            }

            savePath = m_path;
        }

        std::string suffix = io::suffix(savePath);

        Ret ret = saveProject(savePath, suffix);
        if (ret) {
            if (saveMode != SaveMode::SaveCopy) {
                markAsSaved(savePath);
            }
        }

        return ret;
    }
    case SaveMode::AutoSave:
        std::string suffix = io::suffix(path);
        if (suffix == IProjectAutoSaver::AUTOSAVE_SUFFIX) {
            suffix = io::suffix(io::completeBasename(path));
        }

        // if (suffix.empty()) {
        //     // Then it must be a MSCX folder
        //     suffix = engraving::MSCX;
        // }

        return saveProject(path, suffix, false /*generateBackup*/, false /*createThumbnail*/);
    }

    return make_ret(Err::UnknownError);
}

Ret Audacity4Project::saveProject(const muse::io::path_t& path, const std::string& fileSuffix, bool generateBackup, bool createThumbnail)
{
    Q_UNUSED(fileSuffix);
    return doSave(path, generateBackup, createThumbnail);
}

Ret Audacity4Project::doSave(const muse::io::path_t& savePath, bool generateBackup, bool createThumbnail)
{
    TRACEFUNC;

    UNUSED(generateBackup);

    if ((fileSystem()->exists(savePath) && !fileSystem()->isWritable(savePath))) {
        LOGE() << "failed save, not writable path: " << savePath;
        return make_ret(io::Err::FSWriteError);
    }

    auto ret = m_au3Project->save(savePath);
    if (!ret) {
        return make_ret(Ret::Code::UnknownError);
    }

    if (createThumbnail) {
        Ret ret = thumbnailCreator()->createThumbnail(savePath);
        if (!ret) {
            LOGE() << "Failed create thumbnail: " << ret.toString();
            return ret;
        }
    }

    return muse::make_ok();

    //! TODO AU4
    // QString targetContainerPath = engraving::containerPath(path).toQString();
    // muse::io::path_t targetMainFilePath = engraving::mainFilePath(path);
    // muse::io::path_t targetMainFileName = engraving::mainFileName(path);
    // QString savePath = targetContainerPath + "_saving";

    // // Step 1: check writable
    // {
    //     if ((fileSystem()->exists(savePath) && !fileSystem()->isWritable(savePath))
    //         || (fileSystem()->exists(targetContainerPath) && !fileSystem()->isWritable(targetContainerPath))) {
    //         LOGE() << "failed save, not writable path: " << targetContainerPath;
    //         return make_ret(io::Err::FSWriteError);
    //     }

    //     if (ioMode == engraving::MscIoMode::Dir) {
    //         // Dir needs to be created, otherwise we can't move to it
    //         if (!QDir(targetContainerPath).mkpath(".")) {
    //             LOGE() << "Couldn't create container directory: " << targetContainerPath;
    //             return make_ret(io::Err::FSMakingError);
    //         }
    //     }
    // }

    // // Step 2: write project
    // {
    //     MscWriter::Params params;
    //     params.filePath = savePath;
    //     params.mainFileName = targetMainFileName.toQString();
    //     params.mode = ioMode;
    //     IF_ASSERT_FAILED(params.mode != MscIoMode::Unknown) {
    //         return make_ret(Ret::Code::InternalError);
    //     }

    //     MscWriter msczWriter(params);
    //     Ret ret = writeProject(msczWriter, false /*onlySelection*/, createThumbnail);
    //     msczWriter.close();

    //     if (!ret) {
    //         LOGE() << "failed write project to buffer: " << ret.toString();
    //         return ret;
    //     }

    //     if (msczWriter.hasError()) {
    //         LOGE() << "MscWriter has error after writing project";
    //         return make_ret(Ret::Code::UnknownError);
    //     }
    // }

    // // Step 3: create backup if need
    // {
    //     if (generateBackup) {
    //         makeCurrentFileAsBackup();
    //     }
    // }

    // // Step 4: replace to saved file
    // {
    //     if (ioMode == MscIoMode::Dir) {
    //         RetVal<io::paths_t> filesToBeMoved = fileSystem()->scanFiles(savePath, { "*" }, io::ScanMode::FilesAndFoldersInCurrentDir);
    //         if (!filesToBeMoved.ret) {
    //             return filesToBeMoved.ret;
    //         }

    //         Ret ret = muse::make_ok();

    //         for (const muse::io::path_t& fileToBeMoved : filesToBeMoved.val) {
    //             muse::io::path_t destinationFile
    //                 = muse::io::path_t(targetContainerPath).appendingComponent(io::filename(fileToBeMoved));
    //             LOGD() << fileToBeMoved << " to " << destinationFile;
    //             ret = fileSystem()->move(fileToBeMoved, destinationFile, true);
    //             if (!ret) {
    //                 return ret;
    //             }
    //         }

    //         // Try to remove the temp save folder (not problematic if fails)
    //         ret = fileSystem()->remove(savePath, true);
    //         if (!ret) {
    //             LOGW() << ret.toString();
    //         }
    //     } else {
    //         Ret ret = fileSystem()->move(savePath, targetContainerPath, true);
    //         if (!ret) {
    //             return ret;
    //         }
    //     }
    // }

    // // make file readable by all
    // {
    //     QFile::setPermissions(targetMainFilePath.toQString(),
    //                           QFile::ReadOwner | QFile::WriteOwner | QFile::ReadUser | QFile::ReadGroup | QFile::ReadOther);
    // }

    // LOGI() << "success save file: " << targetContainerPath;
    // return make_ret(Ret::Code::Ok);
}

void Audacity4Project::markAsSaved(const muse::io::path_t& path)
{
    //! TODO AU4
    TRACEFUNC;

    //! NOTE: order is important
    m_isNewlyCreated = false;

    setNeedSave(false);

    setPath(path);

    // m_masterNotation->notation()->undoStack()->stackChanged().notify();
}

void Audacity4Project::setNeedSave(bool needSave)
{
    Q_UNUSED(needSave);
    //! TODO AU4
    // mu::engraving::MasterScore* score = m_masterNotation->masterScore();
    // if (!score) {
    //     return;
    // }

    // setNeedAutoSave(needSave);

    // bool saved = !needSave;

    // if (saved) {
    //     m_hasNonUndoStackChanges = false;
    // }

    // if (score->saved() == saved) {
    //     return;
    // }

    // score->setSaved(saved);
    // m_needSaveNotification.notify();
}

const ITrackeditProjectPtr Audacity4Project::trackeditProject() const
{
    return m_trackeditProject;
}

IProjectViewStatePtr Audacity4Project::viewState() const
{
    return m_viewState;
}

uintptr_t Audacity4Project::au3ProjectPtr() const
{
    return m_au3Project->au3ProjectPtr();
}
