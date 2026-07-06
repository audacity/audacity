/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "au3cloud/icloudprojectsprovider.h"

namespace au::au3cloud {
class CloudProjectsProvider : public ICloudProjectsProvider
{
public:
    std::optional<CloudProjectRecord> projectRecordForPath(const muse::io::path_t& projectPath) const override;
    std::optional<CloudProjectRecord> projectRecordForId(const std::string& projectId) const override;
};
}
