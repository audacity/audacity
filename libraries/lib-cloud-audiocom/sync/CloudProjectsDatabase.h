/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectsDatabase.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <optional>
#include <string>

#include "sqlite/SafeConnection.h"

#include "CloudSyncUtils.h"

namespace cloud::audiocom::sync
{
struct DBProjectData final
{
   std::string ProjectId;
   std::string SnapshotId;
   int64_t SavesCount = 0;
   int64_t LastAudioPreview = 0;
   std::string LocalPath;
   int64_t LastModified = 0;
   int64_t LastRead = 0;

   enum SyncStatusType
   {
      SyncStatusSynced = 0,
      SyncStatusUploading = 1,
      SyncStatusDownloading = 2,
   } SyncStatus = {};
};

class CloudProjectsDatabase final
{
   CloudProjectsDatabase();
   ~CloudProjectsDatabase() = default;

public:
   static CloudProjectsDatabase& Get();

   bool IsOpen() const;

   sqlite::SafeConnection::Lock GetConnection();
   const sqlite::SafeConnection::Lock GetConnection() const;

   std::optional<DBProjectData> GetProjectData(const std::string_view& projectId) const;
   std::optional<DBProjectData> GetProjectDataForPath(const std::string& projectPath) const;
   bool MarkProjectAsSynced(const std::string_view& projectId, const std::string_view& snapshotId);

   void UpdateProjectBlockList(const std::string_view& projectId, const SampleBlockIDSet& blockSet);

   std::optional<std::string> GetBlockHash(const std::string_view& projectId, int64_t blockId) const;

   void UpdateBlockHashes(
      const std::string_view& projectId,
      const std::vector<std::pair<int64_t, std::string>>& hashes);

   bool UpdateProjectData(const DBProjectData& projectData);

   std::string GetProjectUserSlug(std::string_view projectId);
   void SetProjectUserSlug(std::string_view projectId, std::string_view slug);

private:
   std::optional<DBProjectData> DoGetProjectData(sqlite::RunResult result) const;
   bool OpenConnection();
   std::shared_ptr<sqlite::SafeConnection> mConnection;

};
} // namespace cloud::audiocom::sync
