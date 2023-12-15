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
#include <vector>
#include <memory>

enum class sampleFormat : unsigned;
class SampleBlock;
using SampleBlockPtr = std::shared_ptr<SampleBlock>;

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

struct ProjectInfo final
{
   std::string Id;

   int64_t Created;
   int64_t Updated;
};

struct SnapshotInfo final
{
   std::string Id;
};

struct ProjectSyncState final
{
   UploadUrls MixdownUrls;
   UploadUrls FileUrls;
   std::vector<UploadUrls> MissingBlocks;
};

struct ProjectResponse final
{
   ProjectInfo Project;
   SnapshotInfo Snapshot;

   ProjectSyncState SyncState;
};

std::optional<ProjectResponse> DeserializeProjectResponse(const std::string& data);

} // namespace cloud::audiocom::sync
