/*!********************************************************************
 Audacity: A Digital Audio Editor
 **********************************************************************/

#include "UpdateFeedMigrationParser.h"

#include <rapidjson/document.h>
#include <rapidjson/error/en.h>

#include <wx/platinfo.h>
#include <wx/log.h>

#include <algorithm>
#include <cctype>

namespace
{
std::string GetString(const rapidjson::Value& obj, const char* key)
{
    if (obj.HasMember(key) && obj[key].IsString())
        return obj[key].GetString();
    return {};
}

std::string GetFileSuffix(const std::string& filename)
{
    auto pos = filename.rfind('.');
    if (pos != std::string::npos) {
        std::string suffix = filename.substr(pos + 1);
        std::transform(suffix.begin(), suffix.end(), suffix.begin(),
                       [](unsigned char c) { return std::tolower(c); });
        return suffix;
    }
    return {};
}
} // namespace

bool UpdateFeedMigrationParser::ParseRelease(const std::string& jsonData, ReleaseInfo &releaseInfo)
{
    rapidjson::Document document;
    document.Parse(jsonData.c_str());

    if (document.HasParseError())
    {
        mLastError = std::string("JSON parse error: ") +
                     rapidjson::GetParseError_En(document.GetParseError());
        wxLogError("UpdateFeedMigrationParser: %s", mLastError);
        return false;
    }

    if (!document.IsObject())
    {
        mLastError = "Expected JSON object at root";
        wxLogError("UpdateFeedMigrationParser: %s", mLastError);
        return false;
    }

    std::string tagName = GetString(document, "tag_name");
    releaseInfo.tagName = tagName;

    // Remove 'Audacity-' prefix if present
    if (tagName.find("Audacity-") == 0)
        tagName = tagName.substr(9);

    releaseInfo.version = tagName;

    // Parse release notes
    releaseInfo.notes = GetString(document, "body");

    std::string targetSuffix = GetPlatformFileSuffix();
    ArchitectureMatch bestMatch = ArchitectureMatch::None;

    if (!document.HasMember("assets") || !document["assets"].IsArray())
        return false;

    const auto& assets = document["assets"];
    for (rapidjson::SizeType i = 0; i < assets.Size(); ++i)
    {
        if (!assets[i].IsObject())
            continue;

        const auto& assetObj = assets[i];
        ReleaseAsset asset;
        asset.name = GetString(assetObj, "name");
        asset.downloadUrl = GetString(assetObj, "browser_download_url");
        asset.arch = GetAssetArchitecture(asset.name);

        releaseInfo.assets.push_back(asset);

        std::string suffix = GetFileSuffix(asset.name);
        if (suffix == targetSuffix)
        {
            ArchitectureMatch match = GetArchitectureMatch(asset.arch);
            if (match == ArchitectureMatch::None)
                continue;

            // Prefer Universal > Native > Fallback
            if (releaseInfo.fileUrl.empty() || match > bestMatch)
            {
                bestMatch = match;
                releaseInfo.fileName = asset.name;
                releaseInfo.fileUrl = asset.downloadUrl;
            }
        }
    }

    return true;
}

bool UpdateFeedMigrationParser::ParseReleasesFeed(const std::string& jsonData, ReleasesFeed& releasesFeed)
{
    rapidjson::Document document;
    document.Parse(jsonData.c_str());

    if (document.HasParseError())
    {
        mLastError = std::string("JSON parse error: ") +
                     rapidjson::GetParseError_En(document.GetParseError());
        wxLogError("UpdateFeedMigrationParser: %s", mLastError);
        return false;
    }

    if (!document.IsObject())
    {
        mLastError = "Expected JSON object at root";
        wxLogError("UpdateFeedMigrationParser: %s", mLastError);
        return false;
    }

    if (!document.HasMember("releases") || !document["releases"].IsArray())
    {
        mLastError = "Missing 'releases' array";
        wxLogError("UpdateFeedMigrationParser: %s", mLastError);
        return false;
    }

    const auto& releases = document["releases"];
    for (rapidjson::SizeType i = 0; i < releases.Size(); ++i)
    {
        if (!releases[i].IsObject())
            continue;

        const auto& releaseObj = releases[i];
        ReleaseInfo info;
        info.version = GetString(releaseObj, "version");
        info.notes = GetString(releaseObj, "notes");

        releasesFeed.releases.push_back(info);
    }

    return true;
}

std::string UpdateFeedMigrationParser::GetPlatformFileSuffix() const
{
    const wxPlatformInfo& info = wxPlatformInfo::Get();

    if (info.GetOperatingSystemId() & wxOS_WINDOWS)
        return "msi";
    else if (info.GetOperatingSystemId() & wxOS_MAC)
        return "dmg";
    else if (info.GetOperatingSystemId() & wxOS_UNIX_LINUX)
        return "appimage";

    return {};
}

std::string UpdateFeedMigrationParser::GetAssetArchitecture(const std::string& assetName) const
{
    std::string nameLower = assetName;
    std::transform(nameLower.begin(), nameLower.end(), nameLower.begin(),
                   [](unsigned char c) { return std::tolower(c); });

    if (nameLower.find("universal") != std::string::npos)
    {
        return "universal";
    }
    else if (nameLower.find("aarch64") != std::string::npos ||
             nameLower.find("arm64") != std::string::npos)
    {
        return "arm64";
    }
    else if (nameLower.find("arm") != std::string::npos)
    {
        return "arm";
    }

    return "x86_64";
}

UpdateFeedMigrationParser::ArchitectureMatch UpdateFeedMigrationParser::GetArchitectureMatch(const std::string& assetArch) const
{
    const wxPlatformInfo& info = wxPlatformInfo::Get();

    // Universal binaries are the best match on macOS
    if (assetArch == "universal")
    {
        if (info.GetOperatingSystemId() & wxOS_MAC)
            return ArchitectureMatch::Universal;
        else
            // Universal is macOS-only
            return ArchitectureMatch::None;
    }

    if (assetArch == "arm64" || assetArch == "aarch64")
    {
        #if defined(__aarch64__) || defined(_M_ARM64)
            return ArchitectureMatch::Native;
        #else
            return ArchitectureMatch::None;
        #endif
    }
    else if (assetArch == "arm")
    {
        #if defined(__arm__) || defined(_M_ARM)
            return ArchitectureMatch::Native;
        #else
            return ArchitectureMatch::None;
        #endif
    }

    // Asset is x86_64
    #if defined(__aarch64__) || defined(_M_ARM64) || defined(__arm__) || defined(_M_ARM)
        // Running on ARM - x86_64 can work via Rosetta/emulation as fallback
        return ArchitectureMatch::Fallback;
    #else
        // Running on x86_64 - native match
        return info.GetArchitecture() == wxARCH_64 ? ArchitectureMatch::Native : ArchitectureMatch::None;
    #endif
}
