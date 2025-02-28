/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSyncDTO.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <string>
#include <string_view>
#include <optional>
#include <unordered_set>
#include <vector>
#include <memory>

#include <wx/string.h>

enum class sampleFormat : unsigned;
class SampleBlock;
using SampleBlockPtr = std::shared_ptr<SampleBlock>;
using SampleBlockID = long long;
using SampleBlockIDSet = std::unordered_set<SampleBlockID>;

namespace audacity::cloud::audiocom::sync {
using BlockID = int64_t;
using BlockHash = std::string;
using SampleBlockHashes = std::vector<BlockHash>;

struct LockedBlock final
{
    BlockID Id;
    sampleFormat Format;
    SampleBlockPtr Block;
    BlockHash Hash;
}; // struct LockedBlock

struct ProjectForm final
{
    std::string Name;
    std::string HeadSnapshotId;
    SampleBlockHashes Hashes;
    bool Force {};
}; // struct ProjectForm

struct UploadUrls final
{
    std::string Id;
    std::string UploadUrl;
    std::string SuccessUrl;
    std::string FailUrl;
}; // struct UploadUrls

struct VersionInfo final
{
    std::string Id;
    std::string Name;
    std::string SnapshotId;
    int64_t Created {};
    int64_t Updated {};
}; // struct VersionInfo

struct SnapshotBlockInfo final
{
    BlockHash Hash;
    std::string Url;
}; // struct SnapshotBlockInfo

struct SnapshotInfo final
{
    std::string Id;
    std::string ParentId;

    int64_t Created {};
    int64_t Updated {};
    int64_t Synced {};

    int64_t FileSize;
    int64_t BlocksSize;

    std::string FileUrl;

    std::vector<SnapshotBlockInfo> Blocks;
}; // struct SnapshotInfo

struct ProjectInfo final
{
    std::string Id;

    std::string Username;
    std::string AuthorName;
    std::string Slug;
    std::string Name;
    std::string Details;

    SnapshotInfo HeadSnapshot;
    std::string LastSyncedSnapshotId;

    int64_t Created {};
    int64_t Updated {};
}; // struct ProjectInfo

struct ProjectSyncState final
{
    UploadUrls MixdownUrls;
    UploadUrls FileUrls;
    std::vector<UploadUrls> MissingBlocks;
}; // struct ProjectSyncState

struct CreateSnapshotResponse final
{
    ProjectInfo Project;
    SnapshotInfo Snapshot;

    ProjectSyncState SyncState;
}; // struct CreateSnapshotResponse

struct PaginationInfo final
{
    int TotalCount {};
    int PagesCount {};
    int CurrentPage {};
    int PageSize {};
}; // struct PaginationInfo

struct PaginatedProjectsResponse final
{
    std::vector<ProjectInfo> Items;
    PaginationInfo Pagination;
}; // struct PaginatedProjectsResponse

struct NetworkStats final
{
    int64_t Files {};
    int64_t Blocks {};
    int64_t Bytes {};
    int64_t Mixes {};
    bool IsDownload {};
}; // struct NetworkStats

std::string Serialize(const ProjectForm& form);

std::optional<ProjectSyncState>
DeserializeProjectSyncState(const std::string& data);

std::optional<CreateSnapshotResponse>
DeserializeCreateSnapshotResponse(const std::string& data);
std::optional<PaginatedProjectsResponse>
DeserializePaginatedProjectsResponse(const std::string& data);

std::optional<ProjectInfo> DeserializeProjectInfo(const std::string& data);
std::optional<SnapshotInfo> DeserializeSnapshotInfo(const std::string& data);

std::string Serialize(NetworkStats stats);

CLOUD_AUDIOCOM_API wxString
MakeSafeProjectPath(const wxString& rootDir, const wxString& projectName);
} // namespace audacity::cloud::audiocom::sync
