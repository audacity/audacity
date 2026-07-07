/*
* Audacity: A Digital Audio Editor
*/
#include "cloudprojectsproviderstub.h"

using namespace au::au3cloud;

std::optional<CloudProjectRecord> CloudProjectsProviderStub::projectRecordForPath(const muse::io::path_t&) const
{
    return std::nullopt;
}

std::optional<CloudProjectRecord> CloudProjectsProviderStub::projectRecordForId(const std::string&) const
{
    return std::nullopt;
}
