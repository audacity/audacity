#pragma once

#include "modularity/ioc.h"
#include "multiwindows/iprojectprovider.h"
#include "context/iglobalcontext.h"
#include "project/types/projecttypes.h"

namespace au::project {
class ProjectProvider : public muse::mi::IProjectProvider, public muse::Contextable
{
    muse::ContextInject<au::context::IGlobalContext> globalContext { this };

public:
    ProjectProvider(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    bool isProjectOpened(const muse::io::path_t& path) const override
    {
        auto project = globalContext()->currentProject();
        if (!project) {
            return false;
        }
        return isSameProjectFile(project->path(), path);
    }

    bool isAnyProjectOpened() const override
    {
        return globalContext()->currentProject() != nullptr;
    }
};
}
