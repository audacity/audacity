/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"

#include "../iprojectviewstatecreator.h"

namespace au::projectscene {
class ProjectViewStateCreator : public IProjectViewStateCreator, public muse::Injectable
{
public:
    ProjectViewStateCreator(const muse::modularity::ContextPtr& ctx)
        : muse::Injectable(ctx) {}

    std::shared_ptr<IProjectViewState> createViewState(std::shared_ptr<au::au3::IAu3Project> project) const override;
};
}
