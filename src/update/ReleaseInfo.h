/*!********************************************************************
 Audacity: A Digital Audio Editor
 **********************************************************************/
#pragma once

#include <string>
#include <vector>
#include <wx/string.h>

struct ReleaseAsset
{
    std::string name;
    std::string downloadUrl;
    std::string arch;
};


struct ReleaseInfo
{
    std::string version;
    std::string tagName;
    std::string notes;

    std::string fileName;
    std::string fileUrl;

    std::vector<ReleaseAsset> assets;

    bool IsValid() const
    {
        return !version.empty() && !fileUrl.empty();
    }
};

struct ReleasesFeed
{
    std::vector<ReleaseInfo> releases;
};
