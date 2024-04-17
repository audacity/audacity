#ifndef AU_PROJECT_IPROJECTCONFIGURATION_H
#define AU_PROJECT_IPROJECTCONFIGURATION_H

#include "modularity/imoduleinterface.h"

#include "global/io/path.h"
#include "global/async/channel.h"
#include "async/notification.h"

namespace au::project {
class IProjectConfiguration : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IProjectConfiguration)

public:

    virtual ~IProjectConfiguration() = default;

    virtual muse::io::path_t recentFilesJsonPath() const = 0;
    virtual muse::ByteArray compatRecentFilesData() const = 0;

    virtual muse::io::path_t userProjectsPath() const = 0;
    virtual void setUserProjectsPath(const muse::io::path_t& path) = 0;
    virtual muse::async::Channel<muse::io::path_t> userProjectsPathChanged() const = 0;
    virtual muse::io::path_t defaultUserProjectsPath() const = 0;

    virtual muse::io::path_t lastOpenedProjectsPath() const = 0;
    virtual void setLastOpenedProjectsPath(const muse::io::path_t& path) = 0;

    virtual muse::io::path_t newProjectTemporaryPath() const = 0;

    virtual int homeProjectsPageTabIndex() const = 0;
    virtual void setHomeProjectsPageTabIndex(int index) = 0;

    enum class HomeProjectsPageViewType {
        Grid,
        List
    };

    virtual HomeProjectsPageViewType homeProjectsPageViewType() const = 0;
    virtual void setHomeProjectsPageViewType(HomeProjectsPageViewType type) = 0;
};
}

#endif // AU_PROJECT_IPROJECTCONFIGURATION_H
