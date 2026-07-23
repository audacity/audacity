/*
* Audacity: A Digital Audio Editor
*/

#pragma once
#include "iaudacityproject.h"

#include "modularity/imoduleinterface.h"
#include "iaudacityproject.h"

namespace au::project {
class IProjectCreator : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IProjectCreator)

public:
    virtual ~IProjectCreator() = default;

    virtual IAudacityProjectPtr newProject() const = 0;
    virtual muse::RetVal<IAudacityProjectPtr> loadProject(const muse::io::path_t& path) const = 0;
};
}
