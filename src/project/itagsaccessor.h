/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"

#include "types/projectmeta.h"

namespace au::project {
class ITagsAccessor : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ITagsAccessor)

public:
    virtual ~ITagsAccessor() = default;

    virtual project::ProjectMeta tags() const = 0;
    virtual void setTags(project::ProjectMeta) = 0;
};
}
