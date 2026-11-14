/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "au3cloud/icloudprojectsprovider.h"

namespace au::au3cloud {
class CloudProjectsProviderStub : public ICloudProjectsProvider
{
public:
    std::optional<CloudProjectRecord> projectRecordForPath(const muse::io::path_t& projectPath) const override;
    std::optional<CloudProjectRecord> projectRecordForId(const std::string& projectId) const override;

    muse::io::path_t makeSafeFilePath(const muse::io::path_t& rootDir, const std::string& fileName,
                                      const std::string& fileExtension) const override;
};
}
