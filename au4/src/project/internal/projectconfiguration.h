#ifndef AU_PROJECT_PROJECTCONFIGURATION_H
#define AU_PROJECT_PROJECTCONFIGURATION_H

#include "../iprojectconfiguration.h"

namespace au::project {
class ProjectConfiguration : public IProjectConfiguration
{
public:
    ProjectConfiguration() = default;

    mu::io::path_t userProjectsPath() const override;
    void setUserProjectsPath(const mu::io::path_t& path) override;
    mu::async::Channel<mu::io::path_t> userProjectsPathChanged() const override;
    mu::io::path_t defaultUserProjectsPath() const override;

    mu::io::path_t lastOpenedProjectsPath() const override;
    void setLastOpenedProjectsPath(const mu::io::path_t& path) override;

private:
    mu::async::Channel<mu::io::path_t> m_userProjectsPathChanged;
};
}

#endif // AU_PROJECT_PROJECTCONFIGURATION_H
