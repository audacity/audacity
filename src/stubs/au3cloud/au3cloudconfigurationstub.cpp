/*
* Audacity: A Digital Audio Editor
*/
#include "au3cloudconfigurationstub.h"

using namespace au::au3cloud;

muse::io::path_t Au3CloudConfigurationStub::cloudProjectsPath() const
{
    return {};
}

void Au3CloudConfigurationStub::setCloudProjectsPath(const muse::io::path_t&)
{
}

std::vector<std::string> Au3CloudConfigurationStub::preferredAudioFormats() const
{
    return {};
}

std::string Au3CloudConfigurationStub::exportConfig(const std::string&) const
{
    return {};
}

bool Au3CloudConfigurationStub::shouldWarnOnSyncError() const
{
    return false;
}

void Au3CloudConfigurationStub::setWarnOnSyncError(bool /*warn*/)
{
}
