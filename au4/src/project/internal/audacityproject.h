#ifndef AU_PROJECT_AUDACITYPROJECT_H
#define AU_PROJECT_AUDACITYPROJECT_H

#include "../iaudacityproject.h"

namespace au::project {
class AudacityProject : public IAudacityProject
{
public:
    AudacityProject();

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

    au::processing::ProcessingProjectPtr m_processingProject;
};
}

#endif // AU_PROJECT_AUDACITYPROJECT_H
