/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"

#include "types/projectmeta.h"

namespace au::project {
class IMetadata : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IMetadata)

public:
    virtual ~IMetadata() = default;

    virtual project::ProjectMeta tags() const = 0;
    virtual void setTags(project::ProjectMeta) = 0;

    virtual std::string buildXml(const project::ProjectMeta&) const = 0;
    virtual project::ProjectMeta parseXml(const std::string& xml) const = 0;
};
}
