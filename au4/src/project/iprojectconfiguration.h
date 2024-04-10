#ifndef AU_PROJECT_IPROJECTCONFIGURATION_H
#define AU_PROJECT_IPROJECTCONFIGURATION_H

#include "modularity/imoduleinterface.h"

#include "global/io/path.h"
#include "global/async/channel.h"

namespace au::project {
class IProjectConfiguration : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IProjectConfiguration)

public:

    virtual ~IProjectConfiguration() = default;

    virtual muse::io::path_t userProjectsPath() const = 0;
    virtual void setUserProjectsPath(const muse::io::path_t& path) = 0;
    virtual muse::async::Channel<muse::io::path_t> userProjectsPathChanged() const = 0;
    virtual muse::io::path_t defaultUserProjectsPath() const = 0;

    virtual muse::io::path_t lastOpenedProjectsPath() const = 0;
    virtual void setLastOpenedProjectsPath(const muse::io::path_t& path) = 0;
};
}

#endif // AU_PROJECT_IPROJECTCONFIGURATION_H
