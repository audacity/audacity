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

muse::io::path_t CloudProjectsProviderStub::makeSafeFilePath(const muse::io::path_t& rootDir, const std::string& fileName,
                                                             const std::string& fileExtension) const
{
    return rootDir.appendingComponent(fileName).appendingSuffix(fileExtension);
}
