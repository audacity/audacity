/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/ioc.h"
#include "framework/cloud/icloudconfiguration.h"

#include "au3cloud/iau3cloudconfiguration.h"

namespace au::au3cloud {
class Au3CloudConfiguration : public IAu3CloudConfiguration
{
    muse::GlobalInject<muse::cloud::ICloudConfiguration> cloudConfiguration;

public:
    void init();
    void onAllInited();

    muse::io::path_t cloudProjectsPath() const override;
    void setCloudProjectsPath(const muse::io::path_t& path) override;

    std::vector<std::string> preferredAudioFormats(bool preferLossless) const override;
    std::string exportConfig(const std::string& mimeType) const override;

    bool shouldWarnOnSyncError() const override;
    void setWarnOnSyncError(bool warn) override;
};
}
