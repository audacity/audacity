/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectsDatabase.cpp

  Dmitry Vedenko

**********************************************************************/
#include "CloudProjectsDatabase.h"

#include <cassert>

#include "AppEvents.h"
#include "CodeConversions.h"
#include "FileNames.h"

namespace cloud::audiocom::sync
{
namespace
{
const auto createTableQuery = R"(
CREATE TABLE IF NOT EXISTS projects
(
   project_id TEXT,
   snapshot_id TEXT,
   saves_count INTEGER,
   last_audio_preview_save INTEGER,
   local_path TEXT,
   last_modified INTEGER,
   last_read INTEGER,
   sync_status INTEGER,
   PRIMARY KEY (project_id)
);

CREATE INDEX IF NOT EXISTS local_path_index ON projects (local_path);

CREATE TABLE IF NOT EXISTS pending_blocks
(
   project_id INTEGER,
   block_id INTEGER,
   PRIMARY KEY (project_id, block_id)
);

CREATE TABLE IF NOT EXISTS block_hashes
(
   project_id INTEGER,
   block_id INTEGER,
   hash TEXT,
   PRIMARY KEY (project_id, block_id)
);

CREATE INDEX IF NOT EXISTS block_hashes_index ON block_hashes (hash);

CREATE TABLE IF NOT EXISTS project_users
(
   project_id INTEGER,
   user_name TEXT,
   PRIMARY KEY (project_id)
);
)";

}

CloudProjectsDatabase::CloudProjectsDatabase()
{
   AppEvents::OnAppInitialized([this] { OpenConnection(); });
}

CloudProjectsDatabase& CloudProjectsDatabase::Get()
{
   static CloudProjectsDatabase instance;
   return instance;
}

bool CloudProjectsDatabase::IsOpen() const
{
   return !!mConnection;
}
sqlite::SafeConnection::Lock CloudProjectsDatabase::GetConnection()
{
   if (!mConnection)
      OpenConnection();

   // It is safe to call lock on a null connection
   return sqlite::SafeConnection::Lock { mConnection };
}

const sqlite::SafeConnection::Lock CloudProjectsDatabase::GetConnection() const
{
   return const_cast<CloudProjectsDatabase*>(this)->GetConnection();
}

std::optional<DBProjectData>
CloudProjectsDatabase::GetProjectData(const std::string_view& projectId) const
{
   auto connection = GetConnection();

   if (!connection)
      return {};

   auto statement = connection->CreateStatement(
      "SELECT project_id, snapshot_id, saves_count, last_audio_preview_save, local_path, last_modified, last_read, sync_status FROM projects WHERE project_id = ? LIMIT 1");

   if (!statement)
      return {};

   return DoGetProjectData(statement->Prepare(projectId).Run());
}

std::optional<DBProjectData> CloudProjectsDatabase::GetProjectDataForPath(
   const std::string& projectFilePath) const
{
   auto connection = GetConnection();

   if (!connection)
      return {};

   auto statement = connection->CreateStatement(
      "SELECT project_id, snapshot_id, saves_count, last_audio_preview_save, local_path, last_modified, last_read, sync_status FROM projects WHERE local_path = ? LIMIT 1");

   if (!statement)
      return {};

   return DoGetProjectData(statement->Prepare(projectFilePath).Run());
}

bool CloudProjectsDatabase::MarkProjectAsSynced(
   const std::string_view& projectId, const std::string_view& snapshotId)
{
   auto connection = GetConnection();

   if (!connection)
      return false;

   auto statement = connection->CreateStatement(
      "UPDATE projects SET sync_status = ? WHERE project_id = ? AND snapshot_id = ?");

   if (!statement)
      return false;

   auto result = statement
                    ->Prepare(
                       static_cast<int>(DBProjectData::SyncStatusSynced),
                       projectId, snapshotId)
                    .Run();

   if (!result.IsOk())
      return false;

   return result.GetModifiedRowsCount() > 0;
}

void CloudProjectsDatabase::UpdateProjectBlockList(
   const std::string_view& projectId, const SampleBlockIDSet& blockSet)
{
   auto connection = GetConnection();

   if (!connection)
      return;

   auto inProjectSet = connection->CreateScalarFunction(
      "inProjectSet", [&blockSet](int64_t blockIndex)
      { return blockSet.find(blockIndex) != blockSet.end(); });

   auto statement = connection->CreateStatement(
      "DELETE FROM block_hashes WHERE project_id = ? AND NOT inProjectSet(block_id)");

   auto result = statement->Prepare(projectId).Run();

   if (!result.IsOk())
   {
      assert(false);
   }
}

std::optional<std::string> CloudProjectsDatabase::GetBlockHash(
   const std::string_view& projectId, int64_t blockId) const
{
   auto connection = GetConnection();

   if (!connection)
      return {};

   auto statement = connection->CreateStatement(
      "SELECT hash FROM block_hashes WHERE project_id = ? AND block_id = ? LIMIT 1");

   if (!statement)
      return {};

   auto result = statement->Prepare(projectId, blockId).Run();

   for (auto row : result)
   {
      std::string hash;

      if (!row.Get(0, hash))
         return {};

      return hash;
   }

   return {};
}

void CloudProjectsDatabase::UpdateBlockHashes(
   const std::string_view& projectId,
   const std::vector<std::pair<int64_t, std::string>>& hashes)
{
   auto connection = GetConnection();

   if (!connection)
      return;

   const int localVar {};
   auto transaction = connection->BeginTransaction(
      std::string("UpdateBlockHashes_") +
      std::to_string(reinterpret_cast<size_t>(&localVar)));

   auto statement = connection->CreateStatement(
      "INSERT OR REPLACE INTO block_hashes (project_id, block_id, hash) VALUES (?, ?, ?)");

   for (const auto& [blockId, hash] : hashes)
      statement->Prepare(projectId, blockId, hash).Run();

   transaction.Commit();
}

bool CloudProjectsDatabase::UpdateProjectData(const DBProjectData& projectData)
{
   auto connection = GetConnection();

   if (!connection)
      return false;

   auto statement = connection->CreateStatement(
      "INSERT OR REPLACE INTO projects (project_id, snapshot_id, saves_count, last_audio_preview_save, local_path, last_modified, last_read, sync_status) VALUES (?, ?, ?, ?, ?, ?, ?, ?)");

   if (!statement)
      return false;

   auto result = statement
                    ->Prepare(
                       projectData.ProjectId, projectData.SnapshotId,
                       projectData.SavesCount, projectData.LastAudioPreview,
                       projectData.LocalPath, projectData.LastModified,
                       projectData.LastRead, projectData.SyncStatus)
                    .Run();

   return result.IsOk();
}

std::string
CloudProjectsDatabase::GetProjectUserSlug(std::string_view projectId)
{
   auto connection = GetConnection();

   if (!connection)
      return {};

   auto statement = connection->CreateStatement(
      "SELECT user_name FROM project_users WHERE project_id = ? LIMIT 1");

   if (!statement)
      return {};

   auto result = statement->Prepare(projectId).Run();

   for (auto row : result)
   {
      std::string slug;

      if (!row.Get(0, slug))
         return {};

      return slug;
   }

   return {};
}

void CloudProjectsDatabase::SetProjectUserSlug(
   std::string_view projectId, std::string_view slug)
{
   auto connection = GetConnection();

   if (!connection)
      return;

   auto statement = connection->CreateStatement(
      "INSERT OR REPLACE INTO project_users (project_id, user_name) VALUES (?, ?)");

   if (!statement)
      return;

   statement->Prepare(projectId, slug).Run();
}

std::optional<DBProjectData>
CloudProjectsDatabase::DoGetProjectData(sqlite::RunResult result) const
{
   for (auto row : result)
   {
      DBProjectData data;

      if (!row.Get(0, data.ProjectId))
         return {};

      if (!row.Get(1, data.SnapshotId))
         return {};

      if (!row.Get(2, data.SavesCount))
         return {};

      if (!row.Get(3, data.LastAudioPreview))
         return {};

      if (!row.Get(4, data.LocalPath))
         return {};

      if (!row.Get(5, data.LastModified))
         return {};

      if (!row.Get(6, data.LastRead))
         return {};

      int status;
      if (!row.Get(7, status))
         return {};

      data.SyncStatus = static_cast<DBProjectData::SyncStatusType>(status);

      return data;
   }

   return {};
}

bool CloudProjectsDatabase::OpenConnection()
{
   if (mConnection)
      return true;

   const auto configDir  = FileNames::ConfigDir();
   const auto configPath = configDir + "/audiocom_sync.db";

   mConnection = sqlite::SafeConnection::Open(audacity::ToUTF8(configPath));

   if (!mConnection)
      return false;

   auto result = mConnection->Acquire()->Execute(createTableQuery);

   if (!result)
   {
      mConnection = {};
      return false;
   }

   return true;
}
} // namespace cloud::audiocom::sync
