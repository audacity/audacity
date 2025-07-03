/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <gmock/gmock.h>

#include "projectscene/iprojectviewstatecreator.h"
#include "au3wrap/iau3project.h"
#include "projectscene/iprojectviewstate.h"

namespace au::projectscene {
class ProjectViewStateCreatorMock final : public IProjectViewStateCreator
{
public:
    MOCK_METHOD(IProjectViewStatePtr, createViewState,
                (std::shared_ptr<au::au3::IAu3Project> au3project),
                (const, override));
};
} // namespace au::projectscene
