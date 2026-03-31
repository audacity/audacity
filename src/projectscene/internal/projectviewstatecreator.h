/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../iprojectviewstatecreator.h"

namespace au::projectscene {
class ProjectViewStateCreator : public IProjectViewStateCreator
{
public:
    std::shared_ptr<IProjectViewState> createViewState(std::shared_ptr<au::au3::IAu3Project> project) const override;
};
}
