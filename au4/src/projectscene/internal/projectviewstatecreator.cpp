/*
* Audacity: A Digital Audio Editor
*/
#include "projectviewstatecreator.h"

#include "projectviewstate.h"

using namespace au::projectscene;

std::shared_ptr<IProjectViewState> ProjectViewStateCreator::createViewState() const
{
    return std::make_shared<ProjectViewState>();
}
