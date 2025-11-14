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
#include <variant>
#include <vector>
#include <memory>

#include <wx/string.h>

enum class sampleFormat : unsigned;
class SampleBlock;
using SampleBlockPtr = std::shared_ptr<SampleBlock>;
using SampleBlockID = long long;
using SampleBlockIDSet = std::unordered_set<SampleBlockID>;

namespace audacity::cloud::audiocom::sync
{

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

struct CloudAudioInfo final
{
   std::string Id;

   std::string Username;
   std::string AuthorName;
   std::string Slug;
   std::string Title;
   std::vector<std::string> Tags;

   int64_t Created {};
}; // struct CloudAudioInfo

struct CloudAudioFullInfo final
{
   std::string Id;

   std::string Username;
   std::string AuthorName;
   std::string AuthorId;
   std::string Slug;
   std::string Title;
   std::vector<std::string> Tags;

   bool IsDownloadable {};
   int64_t Created {};
}; // struct CloudAudioFullInfo


struct CloudAudioDownloadInfoItem final
{
   std::string Format;
   std::string Url;
   int64_t Size {};
   bool IsSource {};
}; // struct CloudAudioDownloadInfoItem

struct CloudAudioDownloadInfo final
{
   std::vector<CloudAudioDownloadInfoItem> Items;
}; // struct CloudAudioDownloadInfo

struct PaginatedAudioResponse final
{
   std::vector<CloudAudioInfo> Items;
   PaginationInfo Pagination;
}; // struct PaginatedAudioResponse

struct NetworkStats final
{
   int64_t Files {};
   int64_t Blocks {};
   int64_t Bytes {};
   int64_t Mixes {};
   bool IsDownload {};
}; // struct NetworkStats

enum class TaskAction
{
   OpenAudio,
   OpenProject,
   Unknown
};

enum class TaskStatus
{
   Pending,
   Started,
   Success,
   Failed,
   Unknown
};

struct ProjectOpenTaskParameters final
{
   std::string ProjectId;
   std::string SnapshotId;
}; // struct ProjectOpenTaskParameters

struct AudioOpenTaskParameters final
{
   std::string AudioId;
}; // struct AudioOpenTaskParameters

struct AppTask final {
   std::string Id;
   TaskStatus Status;
   TaskAction Action;
   int64_t Created {};
   std::variant<ProjectOpenTaskParameters, AudioOpenTaskParameters> Parameters;
}; // struct AppTask

struct PollingInfo final
{
   int IntervalSeconds { 10 };
   bool Stop { false };
}; // struct PollingInfo

struct AppTaskPollResponse final
{
   PollingInfo Polling;
   std::vector<AppTask> Tasks;
}; // struct AppTaskPollResponse

std::string Serialize(const ProjectForm& form);

std::optional<ProjectSyncState>
DeserializeProjectSyncState(const std::string& data);

std::optional<CreateSnapshotResponse>
DeserializeCreateSnapshotResponse(const std::string& data);
std::optional<PaginatedProjectsResponse>
DeserializePaginatedProjectsResponse(const std::string& data);

// Audio
std::optional<PaginatedAudioResponse>
DeserializePaginatedAudioResponse(const std::string& data);

std::optional<CloudAudioDownloadInfo>
DeserializeCloudAudioDownloadInfo(const std::string& data);

std::optional<CloudAudioInfo> DeserializeCloudAudioInfo(const std::string& data);
std::optional<CloudAudioFullInfo> DeserializeCloudAudioFullInfo(const std::string& data);

std::optional<ProjectInfo> DeserializeProjectInfo(const std::string& data);
std::optional<SnapshotInfo> DeserializeSnapshotInfo(const std::string& data);

std::optional<AppTaskPollResponse> DeserializeAppTaskPollResponse(const std::string& data);

std::string Serialize(NetworkStats stats);

CLOUD_AUDIOCOM_API wxString
MakeSafeProjectPath(const wxString& rootDir, const wxString& projectName);

CLOUD_AUDIOCOM_API wxString
MakeSafeFilePath(const wxString& rootDir, const wxString& fileName, const wxString& fileExtension);

} // namespace audacity::cloud::audiocom::sync
