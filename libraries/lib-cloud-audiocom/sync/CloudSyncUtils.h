/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSyncUtils.h

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
};

struct ProjectForm final
{
   std::string Name;
   std::string HeadSnapshotId;
   SampleBlockHashes Hashes;
   bool Force {};
};


std::string Serialize(const ProjectForm& form);

struct UploadUrls final
{
   std::string Id;
   std::string UploadUrl;
   std::string SuccessUrl;
   std::string FailUrl;
};

struct VersionInfo final
{
   std::string Id;
   std::string Name;
   std::string SnapshotId;
   int64_t Created {};
   int64_t Updated {};
};

struct SnapshotBlockInfo final
{
   BlockHash Hash;
   std::string Url;
};

struct SnapshotInfo final
{
   std::string Id;

   int64_t Created {};
   int64_t Updated {};
   int64_t Synced {};

   int64_t FileSize;
   int64_t BlocksSize;

   std::string FileUrl;

   std::vector<SnapshotBlockInfo> Blocks;
};

struct ProjectInfo final
{
   std::string Id;

   std::string Username;
   std::string AuthorName;
   std::string Slug;
   std::string Name;
   std::string Details;

   SnapshotInfo HeadSnapshot;
   std::vector<VersionInfo> Versions;


   int64_t Created {};
   int64_t Updated {};
};

struct ProjectSyncState final
{
   UploadUrls MixdownUrls;
   UploadUrls FileUrls;
   std::vector<UploadUrls> MissingBlocks;
};

struct CreateSnapshotResponse final
{
   ProjectInfo Project;
   SnapshotInfo Snapshot;

   ProjectSyncState SyncState;
};

struct PaginationInfo final
{
   int TotalCount {};
   int PagesCount {};
   int CurrentPage {};
   int PageSize {};
};

struct PaginatedProjectsResponse final
{
   std::vector<ProjectInfo> Items;
   PaginationInfo Pagination;
};

struct NetworkStats final
{
   int64_t Files {};
   int64_t Blocks {};
   int64_t Bytes {};
   int64_t Mixes {};
   bool IsDownload {};
};

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
