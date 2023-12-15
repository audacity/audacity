/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectsDatabase.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <optional>
#include <string>
#include <unordered_set>

#include "sqlite/Connection.h"

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

   sqlite::Connection& GetConnection();
   const sqlite::Connection& GetConnection() const;

   std::optional<DBProjectData> GetProjectDataForPath(const std::string& projectPath) const;

   void UpdateProjectBlockList(const std::string_view& projectId, const std::unordered_set<int64_t> blockSet);

   std::optional<std::string> GetBlockHash(const std::string_view& projectId, int64_t blockId) const;

   void UpdateBlockHashes(
      const std::string_view& projectId,
      const std::vector<std::pair<int64_t, std::string>>& hashes);

   bool OnProjectSnapshotCreated(const DBProjectData& projectData);

private:
   bool OpenConnection();
   sqlite::Connection mConnection;

};
} // namespace cloud::audiocom::sync
