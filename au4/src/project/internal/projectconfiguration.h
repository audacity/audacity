#ifndef AU_PROJECT_PROJECTCONFIGURATION_H
#define AU_PROJECT_PROJECTCONFIGURATION_H

#include "types/val.h"

#include "../iprojectconfiguration.h"

namespace au::project {
class ProjectConfiguration : public IProjectConfiguration
{
public:
    ProjectConfiguration() = default;

    void init();

    muse::io::path_t userProjectsPath() const override;
    void setUserProjectsPath(const muse::io::path_t& path) override;
    muse::async::Channel<muse::io::path_t> userProjectsPathChanged() const override;
    muse::io::path_t defaultUserProjectsPath() const override;

    muse::io::path_t lastOpenedProjectsPath() const override;
    void setLastOpenedProjectsPath(const muse::io::path_t& path) override;

    int homeProjectsPageTabIndex() const override;
    void setHomeProjectsPageTabIndex(int index) override;

    HomeProjectsPageViewType homeProjectsPageViewType() const override;
    void setHomeProjectsPageViewType(HomeProjectsPageViewType type) override;

private:
    muse::async::Channel<muse::io::path_t> m_userProjectsPathChanged;

    int m_homeProjectsPageTabIndex = 0;
    muse::async::Notification m_homeProjectsPageTabIndexChanged;
};
}

#endif // AU_PROJECT_PROJECTCONFIGURATION_H
