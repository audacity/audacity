#ifndef AU_PROJECT_AUDACITYPROJECT_H
#define AU_PROJECT_AUDACITYPROJECT_H

#include "../iaudacityproject.h"
#include "au3wrap/audacity3project.h"

namespace au::au3 {
class Audacity3Project;
}

namespace au::project {
class Audacity4Project : public IAudacityProject
{
public:
    Audacity4Project();

    static IAudacityProjectPtr makeMock();

    mu::Ret load(const mu::io::path_t& path, bool forceMode = false, const std::string& format = "") override;

    mu::io::path_t path() const override { return m_path; }
    mu::async::Notification pathChanged() const override { return m_pathChanged; }

    const au::processing::ProcessingProjectPtr processingProject() const override;

private:
    void setPath(const mu::io::path_t& path);

    mu::Ret doLoad(const mu::io::path_t& path, bool forceMode, const std::string& format);

    mu::io::path_t m_path;
    mu::async::Notification m_pathChanged;

    std::shared_ptr<au::au3::Audacity3Project> m_au3Project;

    au::processing::ProcessingProjectPtr m_processingProject;
};
}

#endif // AU_PROJECT_AUDACITYPROJECT_H
