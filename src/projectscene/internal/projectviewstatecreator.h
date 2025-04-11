/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../iprojectviewstatecreator.h"

namespace au::projectscene {
class ProjectViewStateCreator : public IProjectViewStateCreator
{
public:
    ProjectViewStateCreator() = default;

    std::shared_ptr<IProjectViewState> createViewState(std::shared_ptr<au::au3::IAu3Project> project) const override;
};
}
