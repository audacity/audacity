/*
* Audacity: A Digital Audio Editor
*/
#include "au3cloudconfiguration.h"

#include "framework/global/io/dir.h"

#include "au3-cloud-audiocom/CloudLibrarySettings.h"
#include "au3-cloud-audiocom/ServiceConfig.h"

using namespace au::au3cloud;

muse::io::path_t Au3CloudConfiguration::cloudProjectsPath() const
{
    return muse::io::Dir::fromNativeSeparators(muse::io::path_t(audacity::cloud::audiocom::CloudProjectsSavePath.Read()));
}

void Au3CloudConfiguration::setCloudProjectsPath(const muse::io::path_t& path)
{
    audacity::cloud::audiocom::CloudProjectsSavePath.Write(path.toStdString());
}

std::vector<std::string> Au3CloudConfiguration::preferredAudioFormats() const
{
    return audacity::cloud::audiocom::GetServiceConfig().GetPreferredAudioFormats(true);
}

std::string Au3CloudConfiguration::exportConfig(const std::string& mimeType) const
{
    return audacity::cloud::audiocom::GetServiceConfig().GetExportConfig(mimeType);
}
