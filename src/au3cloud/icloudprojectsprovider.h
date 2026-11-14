/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <optional>
#include <string>

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/io/path.h"

#include "cloudtypes.h"

namespace au::au3cloud {
class ICloudProjectsProvider : MODULE_GLOBAL_EXPORT_INTERFACE
{
    INTERFACE_ID(ICloudProjectsProvider)

public:
    virtual ~ICloudProjectsProvider() = default;

    virtual std::optional<CloudProjectRecord> projectRecordForPath(const muse::io::path_t& projectPath) const = 0;
    virtual std::optional<CloudProjectRecord> projectRecordForId(const std::string& projectId) const = 0;

    virtual muse::io::path_t makeSafeFilePath(const muse::io::path_t& rootDir, const std::string& fileName,
                                              const std::string& fileExtension) const = 0;
};
}
