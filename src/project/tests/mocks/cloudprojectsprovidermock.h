/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <gmock/gmock.h>

#include "au3cloud/icloudprojectsprovider.h"

namespace au::au3cloud {
class CloudProjectsProviderMock : public ICloudProjectsProvider
{
public:
    MOCK_METHOD(std::optional<CloudProjectRecord>, projectRecordForPath, (const muse::io::path_t& projectPath), (const, override));
    MOCK_METHOD(std::optional<CloudProjectRecord>, projectRecordForId, (const std::string& projectId), (const, override));
    MOCK_METHOD(muse::io::path_t, makeSafeFilePath,
                (const muse::io::path_t& rootDir, const std::string& fileName, const std::string& fileExtension), (const, override));
};
}
