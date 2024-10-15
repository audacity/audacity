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

    std::shared_ptr<IProjectViewState> createViewState() const override;
};
}
