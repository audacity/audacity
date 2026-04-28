/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>
#include <vector>

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

    virtual std::vector<std::string> preferredAudioFormats() const = 0;
    virtual std::string exportConfig(const std::string& mimeType) const = 0;

    virtual bool shouldWarnOnSyncError() const = 0;
    virtual void setWarnOnSyncError(bool warn) = 0;
};
}
