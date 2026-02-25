#pragma once

#include "modularity/ioc.h"
#include "multiwindows/iprojectprovider.h"
#include "context/iglobalcontext.h"

namespace au::project {
class ProjectProvider : public muse::mi::IProjectProvider, public muse::Injectable
{
    muse::Inject<au::context::IGlobalContext> globalContext { this };

public:
    ProjectProvider(const muse::modularity::ContextPtr& ctx)
        : muse::Injectable(ctx) {}

    bool isProjectOpened(const muse::io::path_t& path) const override
    {
        auto project = globalContext()->currentProject();
        if (!project) {
            return false;
        }
        return project->path() == path;
    }

    bool isAnyProjectOpened() const override
    {
        return globalContext()->currentProject() != nullptr;
    }
};
}
