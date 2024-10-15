#pragma once

#include "modularity/imoduleinterface.h"

#include "iprojectviewstate.h"

namespace au::projectscene {
class IProjectViewStateCreator : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IProjectViewStateCreator)
public:
    virtual ~IProjectViewStateCreator() = default;

    virtual IProjectViewStatePtr createViewState() const = 0;
};
}
