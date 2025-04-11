#pragma once

#include "modularity/imoduleinterface.h"
#include "au3wrap/iau3project.h"

#include "iprojectviewstate.h"

namespace au::projectscene {
class IProjectViewStateCreator : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IProjectViewStateCreator)
public:
    virtual ~IProjectViewStateCreator() = default;

    virtual IProjectViewStatePtr createViewState(std::shared_ptr<au::au3::IAu3Project> au3project) const = 0;
};
}
