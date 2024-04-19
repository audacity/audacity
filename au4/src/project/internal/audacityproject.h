#ifndef AU_PROJECT_AUDACITYPROJECT_H
#define AU_PROJECT_AUDACITYPROJECT_H

#include "../iaudacityproject.h"
#include "au3wrap/audacity3project.h"
#include "modularity/ioc.h"
#include "io/ifilesystem.h"
// #include "../iprojectconfiguration.h"
// #include "types/projecttypes.h"

namespace au::au3 {
class Audacity3Project;
}

namespace au::project {
//! NOTE There are quite a lot of "projects" right now, it can get confusing
//! The main project of AU4 is au::project::Audacity4Project
//! It contains (will contain) a processing project (au::processing::ProcessingProject)
//! and various settings that are not directly related to processing
//! (for example, mixer settings, selected effects, etc.).
//! A processing project is the main project for processing samples, tracks, operations on them...
//!
//! Perhaps now the division into two projects seems unnecessary.
//! But in the future, there will be a lot of data that needs
//! to be stored with the project (even covers, notes, etc.),
//! which are not related to processing.
//! At the same time, the processing project will be inherently complex,
//! so we want to simplify it and remove from it everything that does not concern it.
//!
//! There is also a project from AU3 (::AudacityProject),
//! through which we receive a lot of data using the AU3 implementation.
//! And a wrapper in AU4 for this project (au::au3::Audacity3Project)
//! A wrapper is needed for two reasons:
//! * Technical - the AU3 code is strongly linked at the linking level,
//! it cannot be used in parts, therefore, to avoid duplication of symbols,
//! we can add (as source) it only to one library
//! * Thanks to this wrapper we will see exactly what we are using from AU3
class Audacity4Project : public IAudacityProject
{
    INJECT(muse::io::IFileSystem, fileSystem)

public:
    Audacity4Project();

    muse::Ret load(const muse::io::path_t& path, bool forceMode = false, const std::string& format = "") override;
    void close() override;

    muse::io::path_t path() const override { return m_path; }
    muse::async::Notification pathChanged() const override { return m_pathChanged; }

    bool isNewlyCreated() const override;
    bool isImported() const override;

    muse::ValNt<bool> needSave() const override;
    muse::Ret canSave() const override;

    bool needAutoSave() const override;
    void setNeedAutoSave(bool val) override;

    muse::Ret save(const muse::io::path_t& path = muse::io::path_t(), SaveMode saveMode = SaveMode::Save) override;

    const au::processing::ProcessingProjectPtr processingProject() const override;

private:
    void setPath(const muse::io::path_t& path);

    muse::Ret doLoad(const muse::io::path_t& path, bool forceMode, const std::string& format);

    muse::Ret saveProject(const muse::io::path_t& path, const std::string& fileSuffix, bool generateBackup = true,
                          bool createThumbnail = true);
    muse::Ret doSave(const muse::io::path_t& path, /*engraving::MscIoMode ioMode,*/ bool generateBackup = true,
                     bool createThumbnail = true);

    void markAsSaved(const muse::io::path_t& path);

    muse::io::path_t m_path;
    muse::async::Notification m_pathChanged;

    bool m_isNewlyCreated = false; /// true if the file has never been saved yet
    bool m_isImported = false;
    bool m_needAutoSave = false;

    std::shared_ptr<au::au3::Audacity3Project> m_au3Project;

    au::processing::ProcessingProjectPtr m_processingProject;
};
}

#endif // AU_PROJECT_AUDACITYPROJECT_H
