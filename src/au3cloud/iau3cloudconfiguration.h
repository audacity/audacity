/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/io/path.h"

namespace au::au3cloud {
class IAu3CloudConfiguration : MODULE_GLOBAL_EXPORT_INTERFACE
{
    INTERFACE_ID(IAu3CloudConfiguration)

public:
    virtual ~IAu3CloudConfiguration() = default;

    virtual muse::io::path_t cloudProjectsPath() const = 0;
    virtual void setCloudProjectsPath(const muse::io::path_t& path) = 0;
};
}
