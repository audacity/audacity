/*
* Audacity: A Digital Audio Editor
*/
#include "projectviewstatecreator.h"

#include "projectviewstate.h"

using namespace au::projectscene;

std::shared_ptr<IProjectViewState> ProjectViewStateCreator::createViewState(std::shared_ptr<au::au3::IAu3Project> project) const
{
    return std::make_shared<ProjectViewState>(project);
}
