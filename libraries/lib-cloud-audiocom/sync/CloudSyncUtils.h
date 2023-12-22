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

namespace cloud::audiocom::sync
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
};


std::string SerializeProjectForm(const ProjectForm& form);

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

struct CreateProjectResponse final
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

std::optional<CreateProjectResponse>
DeserializeCreateProjectResponse(const std::string& data);
std::optional<PaginatedProjectsResponse>
DeserializePaginatedProjectsResponse(const std::string& data);

std::optional<ProjectInfo> DeserializeProjectInfo(const std::string& data);
std::optional<SnapshotInfo> DeserializeSnapshotInfo(const std::string& data);

wxString
MakeSafeProjectPath(const wxString& rootDir, const wxString& projectName);

} // namespace cloud::audiocom::sync
