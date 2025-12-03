/*!********************************************************************
 Audacity: A Digital Audio Editor
 **********************************************************************/
#pragma once

#include <string>

#include "ReleaseInfo.h"

/**
 * This parser handles the new JSON-based (GitHub release) update feed format
 * that is used in Audacity 4.
 */
class UpdateFeedMigrationParser final
{
public:
    UpdateFeedMigrationParser() = default;
    ~UpdateFeedMigrationParser() = default;

    bool ParseRelease(const std::string& jsonData, ReleaseInfo& releaseInfo);
    bool ParseReleasesFeed(const std::string& jsonData, ReleasesFeed& releasesFeed);

    const std::string& GetLastError() const { return mLastError; }

private:
    enum class ArchitectureMatch
    {
        None,      // Asset is not compatible with current architecture
        Fallback,  // Asset can run via emulation (Rosetta2, etc.)
        Native,    // Asset matches current architecture natively
        Universal  // Universal binary (macOS) - works natively on all architectures
    };

    std::string GetPlatformFileSuffix() const;
    std::string GetAssetArchitecture(const std::string& assetName) const;
    ArchitectureMatch GetArchitectureMatch(const std::string& assetArch) const;

    std::string mLastError;
};
