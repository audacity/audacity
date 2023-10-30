/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectSnapshot.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <functional>
#include <memory>
#include <string_view>

#include "CloudSyncUtils.h"

class AudacityProject;

namespace cloud::audiocom::sync
{
constexpr auto UNASSIGNED_PROJECT_ID = -1;

struct SnapshotOperationResult final
{
   std::string errorMessage;
   bool success { false };
};

class MissingBlocksUploader;

using SnapshotOperationCompleted = std::function<void(const SnapshotOperationResult&)>;

class CloudProjectSnapshot final
{
public:
   explicit CloudProjectSnapshot(AudacityProject& project);
   ~CloudProjectSnapshot();

   void SaveSnapshot(SnapshotOperationCompleted callback);

private:
   SampleBlockIDs GetDBPendingBlocks() const;
   void ModifyPendingBlocks(std::string_view query, const SampleBlockIDs& blocks);
   void UpdateDBPendingBlocks(const SampleBlockIDs& blocks);

   void CreateOrUpdateProject();

   std::shared_ptr<AudacityProject> mProject;

   int64_t mCloudProjectId { UNASSIGNED_PROJECT_ID };
   int64_t mCloudVersion { 0 };

   SampleBlockIDs mProjectBlocks;
   std::vector<MissingBlock> mMissingBlocks;
   std::unique_ptr<MissingBlocksUploader> mMissingBlockUploader;

   SnapshotOperationCompleted mCompletedCallback;
};
} // namespace cloud::audiocom::sync
