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
