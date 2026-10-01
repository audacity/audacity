/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "au3cloud/iau3cloudconfiguration.h"

namespace au::au3cloud {
class Au3CloudConfiguration : public IAu3CloudConfiguration
{
public:
    void init();

    muse::io::path_t cloudProjectsPath() const override;
    void setCloudProjectsPath(const muse::io::path_t& path) override;

    std::vector<std::string> preferredAudioFormats(bool preferLossless) const override;
    std::string exportConfig(const std::string& mimeType) const override;

    bool shouldWarnOnSyncError() const override;
    void setWarnOnSyncError(bool warn) override;

    void setSyncDatabasePath(const muse::io::path_t& path) override;
    void setIsOtherCheckout(bool isOtherCheckout) override;
    bool isOtherCheckout() const override;

    void setAccessTokenFile(const muse::io::path_t& path) override;
    muse::io::path_t accessTokenFile() const override;

private:
    bool m_isOtherCheckout = false;
    muse::io::path_t m_accessTokenFile;
};
}
