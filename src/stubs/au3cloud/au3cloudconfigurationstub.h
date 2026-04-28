/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "au3cloud/iau3cloudconfiguration.h"

namespace au::au3cloud {
class Au3CloudConfigurationStub : public IAu3CloudConfiguration
{
public:
    muse::io::path_t cloudProjectsPath() const override;
    void setCloudProjectsPath(const muse::io::path_t& path) override;

    std::vector<std::string> preferredAudioFormats() const override;
    std::string exportConfig(const std::string& mimeType) const override;

    bool shouldWarnOnSyncError() const override;
    void setWarnOnSyncError(bool warn) override;
};
}
