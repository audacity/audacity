#ifndef AU_PROJECT_AUDACITYPROJECT_H
#define AU_PROJECT_AUDACITYPROJECT_H

#include "../iaudacityproject.h"
#include "../ithumbnailcreator.h"
#include "context/iglobalcontext.h"
#include "modularity/ioc.h"
#include "au3wrap/iau3project.h"
#include "io/ifilesystem.h"
#include "projectscene/iprojectviewstatecreator.h"
#include "trackedit/iprojecthistory.h"
#include "trackedit/itrackeditclipboard.h"
#include "trackedit/itrackeditproject.h"

namespace au::au3 {
class Au3ProjectAccessor;
}

namespace au::project {
//! NOTE There are quite a lot of "projects" right now, it can get confusing
//! The main project of AU4 is au::project::Audacity4Project
//! It contains a trackedit project (au::trackedit::TrackeditProject)
//! and various settings that are not directly related to trackedit
//! (for example, mixer settings, selected effects, etc.).
//! A trackedit project is the main project for editing of samples, tracks, operations on them...
//!
//! Perhaps now the division into two projects seems unnecessary.
//! But in the future, there will be a lot of data that needs
//! to be stored with the project (even covers, notes, etc.),
//! which are not related to tracks edit.
//! At the same time, the trackedit project will be inherently complex,
//! so we want to simplify it and remove from it everything that does not concern it.
//!
//! There is also a project from AU3 (::AudacityProject),
//! through which we receive a lot of data using the AU3 implementation.
//! And a wrapper in AU4 for this project (au::au3::Au3Project)
//! A wrapper is needed for two reasons:
//! * Technical - the AU3 code is strongly linked at the linking level,
//! it cannot be used in parts, therefore, to avoid duplication of symbols,
//! we can add (as source) it only to one library
//! * Thanks to this wrapper we will see exactly what we are using from AU3
class Audacity4Project : public IAudacityProject, public muse::async::Asyncable
{
    muse::Inject<au3::IAu3ProjectCreator> au3ProjectCreator;
    muse::Inject<trackedit::ITrackeditProjectCreator> trackeditProjectCreator;
    muse::Inject<muse::io::IFileSystem> fileSystem;
    muse::Inject<projectscene::IProjectViewStateCreator> viewStateCreator;
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<au::trackedit::IProjectHistory> projectHistory;
    muse::Inject<au::trackedit::ITrackeditClipboard> clipboard;
    muse::Inject<IThumbnailCreator> thumbnailCreator;

public:
    Audacity4Project();

    muse::Ret createNew() override;

    muse::Ret load(const muse::io::path_t& path, bool forceMode = false, const std::string& format = "") override;
    void close() override;
    muse::async::Notification aboutCloseBegin() const override;
    muse::async::Notification aboutCloseEnd() const override;

    QString displayName() const override;
    muse::async::Notification displayNameChanged() const override;

    muse::io::path_t path() const override { return m_path; }
    muse::async::Notification pathChanged() const override { return m_pathChanged; }

    bool isNewlyCreated() const override;
    bool isImported() const override;

    muse::ValNt<bool> needSave() const override;
    muse::Ret canSave() const override;

    bool needAutoSave() const override;
    void setNeedAutoSave(bool val) override;

    muse::Ret save(const muse::io::path_t& path = muse::io::path_t(), SaveMode saveMode = SaveMode::Save) override;

    const au::trackedit::ITrackeditProjectPtr trackeditProject() const override;

    projectscene::IProjectViewStatePtr viewState() const override;

    uintptr_t au3ProjectPtr() const override;

private:
    void setPath(const muse::io::path_t& path);

    muse::Ret doLoad(const muse::io::path_t& path, bool forceMode, const std::string& format);

    muse::Ret saveProject(const muse::io::path_t& path, const std::string& fileSuffix, bool generateBackup = true,
                          bool createThumbnail = true);
    muse::Ret doSave(const muse::io::path_t& path, /*engraving::MscIoMode ioMode,*/ bool generateBackup = true,
                     bool createThumbnail = true);

    void markAsSaved(const muse::io::path_t& path);
    void setNeedSave(bool needSave);

    muse::async::Notification m_aboutCloseBegin;
    muse::async::Notification m_aboutCloseEnd;

    muse::io::path_t m_path;
    muse::async::Notification m_pathChanged;
    muse::async::Notification m_displayNameChanged;

    bool m_isNewlyCreated = false; /// true if the file has never been saved yet
    bool m_isImported = false;
    bool m_needAutoSave = false;

    std::shared_ptr<au::au3::IAu3Project> m_au3Project;

    trackedit::ITrackeditProjectPtr m_trackeditProject;

    projectscene::IProjectViewStatePtr m_viewState;
};
}

#endif // AU_PROJECT_AUDACITYPROJECT_H
