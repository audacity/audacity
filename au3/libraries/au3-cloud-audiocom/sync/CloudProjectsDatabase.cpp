/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectsDatabase.cpp

  Dmitry Vedenko

**********************************************************************/
#include "CloudProjectsDatabase.h"

#include <cassert>

#include "CodeConversions.h"
#include "FileNames.h"

namespace audacity::cloud::audiocom::sync {
namespace {
const char* createTableQuery
    =
        R"(
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

CREATE TABLE IF NOT EXISTS block_hashes
(
   project_id INTEGER,
   block_id INTEGER,
   hash TEXT,
   PRIMARY KEY (project_id, block_id)
);

CREATE INDEX IF NOT EXISTS block_hashes_index ON block_hashes (hash);

CREATE TABLE IF NOT EXISTS pending_snapshots
(
   project_id TEXT,
   snapshot_id TEXT,
   confirm_url TEXT,
   PRIMARY KEY (project_id, snapshot_id)
);

CREATE TABLE IF NOT EXISTS pending_project_blobs
(
   project_id TEXT,
   snapshot_id TEXT,

   upload_url TEXT,
   confirm_url TEXT,
   fail_url TEXT,

   blob BLOB,
   PRIMARY KEY (project_id, snapshot_id)
);

CREATE TABLE IF NOT EXISTS pending_project_blocks
(
   project_id TEXT,
   snapshot_id TEXT,

   upload_url TEXT,
   confirm_url TEXT,
   fail_url TEXT,

   block_id INTEGER,
   block_sample_format INTEGER,
   block_hash TEXT,
   PRIMARY KEY (project_id, snapshot_id, block_id)
);

CREATE TABLE IF NOT EXISTS project_users
(
   project_id INTEGER,
   user_name TEXT,
   PRIMARY KEY (project_id)
);

CREATE TABLE IF NOT EXISTS migration
(
   version INTEGER
);

INSERT INTO migration (version) VALUES (0);

DELETE FROM migration WHERE ROWID != 1;
)";

const auto addProjectSyncedDialogShownColumn = R"(
ALTER TABLE projects ADD COLUMN synced_dialog_shown INTEGER DEFAULT 1;
)";

const char* migrations[] = {
    addProjectSyncedDialogShownColumn,
};
} // namespace

CloudProjectsDatabase& CloudProjectsDatabase::Get()
{
    static CloudProjectsDatabase instance;
    return instance;
}

sqlite::SafeConnection::Lock CloudProjectsDatabase::GetConnection()
{
    auto lock = std::lock_guard { mConnectionMutex };

    if (!mConnection) {
        OpenConnection();
    }

    // It is safe to call lock on a null connection
    return sqlite::SafeConnection::Lock { mConnection };
}

const sqlite::SafeConnection::Lock CloudProjectsDatabase::GetConnection() const
{
    return const_cast<CloudProjectsDatabase*>(this)->GetConnection();
}

std::optional<DBProjectData>
CloudProjectsDatabase::GetProjectData(std::string_view projectId) const
{
    auto connection = GetConnection();

    if (!connection) {
        return {}
    }

    auto statement = connection->CreateStatement(
        "SELECT project_id, snapshot_id, saves_count, last_audio_preview_save, local_path, last_modified, last_read, sync_status, synced_dialog_shown FROM projects WHERE project_id = ? LIMIT 1");

    if (!statement) {
        return {}
    }

    return DoGetProjectData(statement->Prepare(projectId).Run());
}

std::optional<DBProjectData> CloudProjectsDatabase::GetProjectDataForPath(
    const std::string& projectFilePath) const
{
    auto connection = GetConnection();

    if (!connection) {
        return {}
    }

    auto statement = connection->CreateStatement(
        "SELECT project_id, snapshot_id, saves_count, last_audio_preview_save, local_path, last_modified, last_read, sync_status, synced_dialog_shown FROM projects WHERE local_path = ? LIMIT 1");

    if (!statement) {
        return {}
    }

    return DoGetProjectData(statement->Prepare(projectFilePath).Run());
}

std::vector<DBProjectData>
cloud::audiocom::sync::CloudProjectsDatabase::GetCloudProjects() const
{
    std::vector<DBProjectData> result;

    auto connection = GetConnection();

    if (!connection) {
        return result;
    }

    auto statement = connection->CreateStatement(
        "SELECT project_id, snapshot_id, saves_count, last_audio_preview_save, local_path, last_modified, last_read, sync_status, synced_dialog_shown FROM projects");

    if (!statement) {
        return result;
    }

    auto runResult = statement->Prepare().Run();

    for (auto row : runResult) {
        auto data = DoGetProjectData(row);

        if (data) {
            result.push_back(*data);
        }
    }

    return result;
}

void cloud::audiocom::sync::CloudProjectsDatabase::DeleteProject(
    std::string_view projectId)
{
    auto connection = GetConnection();

    if (!connection) {
        return;
    }

    auto tx = connection->BeginTransaction("DeleteProject");
    if (DeleteProject(connection, projectId)) {
        tx.Commit();
    }
}

bool CloudProjectsDatabase::MarkProjectAsSynced(
    std::string_view projectId, std::string_view snapshotId)
{
    auto connection = GetConnection();

    if (!connection) {
        return false;
    }

    auto statement = connection->CreateStatement(
        "UPDATE projects SET sync_status = ? WHERE project_id = ? AND snapshot_id = ?");

    if (!statement) {
        return false;
    }

    auto result = statement
                  ->Prepare(
        static_cast<int>(DBProjectData::SyncStatusSynced),
        projectId, snapshotId)
                  .Run();

    if (!result.IsOk()) {
        return false;
    }

    return result.GetModifiedRowsCount() > 0;
}

void CloudProjectsDatabase::UpdateProjectBlockList(
    std::string_view projectId, const SampleBlockIDSet& blockSet)
{
    auto connection = GetConnection();

    if (!connection) {
        return;
    }

    auto inProjectSet = connection->CreateScalarFunction(
        "inProjectSet", [&blockSet](int64_t blockIndex)
    { return blockSet.find(blockIndex) != blockSet.end(); });

    auto statement = connection->CreateStatement(
        "DELETE FROM block_hashes WHERE project_id = ? AND NOT inProjectSet(block_id)");

    auto result = statement->Prepare(projectId).Run();

    if (!result.IsOk()) {
        assert(false);
    }
}

std::optional<std::string> CloudProjectsDatabase::GetBlockHash(
    std::string_view projectId, int64_t blockId) const
{
    auto connection = GetConnection();

    if (!connection) {
        return {}
    }

    auto statement = connection->CreateStatement(
        "SELECT hash FROM block_hashes WHERE project_id = ? AND block_id = ? LIMIT 1");

    if (!statement) {
        return {}
    }

    auto result = statement->Prepare(projectId, blockId).Run();

    for (auto row : result) {
        std::string hash;

        if (!row.Get(0, hash)) {
            return {}
        }

        return hash;
    }

    return {};
}

void CloudProjectsDatabase::UpdateBlockHashes(
    std::string_view projectId,
    const std::vector<std::pair<int64_t, std::string> >& hashes)
{
    auto connection = GetConnection();

    if (!connection) {
        return;
    }

    const int localVar {};
    auto transaction = connection->BeginTransaction(
        std::string("UpdateBlockHashes_")
        + std::to_string(reinterpret_cast<size_t>(&localVar)));

    auto statement = connection->CreateStatement(
        "INSERT OR REPLACE INTO block_hashes (project_id, block_id, hash) VALUES (?, ?, ?)");

    for (const auto& [blockId, hash] : hashes) {
        statement->Prepare(projectId, blockId, hash).Run();
    }

    transaction.Commit();
}

bool CloudProjectsDatabase::UpdateProjectData(const DBProjectData& projectData)
{
    auto connection = GetConnection();

    if (!connection) {
        return false;
    }

    auto tx = connection->BeginTransaction("UpdateProjectData");

    auto updateProjectData = connection->CreateStatement(
        "INSERT OR REPLACE INTO projects (project_id, snapshot_id, saves_count, last_audio_preview_save, local_path, last_modified, last_read, sync_status, synced_dialog_shown) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)");

    if (!updateProjectData) {
        return false;
    }

    auto result = updateProjectData
                  ->Prepare(
        projectData.ProjectId, projectData.SnapshotId,
        projectData.SavesCount, projectData.LastAudioPreview,
        projectData.LocalPath, projectData.LastModified,
        projectData.LastRead, projectData.SyncStatus,
        projectData.FirstSyncDialogShown)
                  .Run();

    if (!result.IsOk()) {
        return false;
    }

    auto listMissingProjects = connection->CreateStatement(
        "SELECT project_id FROM projects WHERE project_id != ? AND local_path = ?");

    if (!listMissingProjects) {
        return false;
    }

    auto missingProjects = listMissingProjects->Prepare(
        projectData.ProjectId, projectData.LocalPath).Run();

    for (auto row : missingProjects) {
        std::string missingProjectId;

        if (!row.Get(0, missingProjectId)) {
            return false;
        }

        if (!DeleteProject(connection, missingProjectId)) {
            return false;
        }
    }

    return tx.Commit().IsOk();
}

bool cloud::audiocom::sync::CloudProjectsDatabase::IsFirstSyncDialogShown(
    std::string_view projectId) const
{
    auto connection = GetConnection();

    if (!connection) {
        return false;
    }

    auto statement = connection->CreateStatement(
        "SELECT synced_dialog_shown FROM projects WHERE project_id = ? LIMIT 1");

    if (!statement) {
        return false;
    }

    auto result = statement->Prepare(projectId).Run();

    for (auto row : result) {
        bool shown;

        if (!row.Get(0, shown)) {
            return false;
        }

        return shown;
    }

    return false;
}

void cloud::audiocom::sync::CloudProjectsDatabase::SetFirstSyncDialogShown(
    std::string_view projectId, bool shown)
{
    auto connection = GetConnection();

    if (!connection) {
        return;
    }

    auto statement = connection->CreateStatement(
        "UPDATE projects SET synced_dialog_shown = ? WHERE project_id = ?");

    if (!statement) {
        return;
    }

    statement->Prepare(shown, projectId).Run();
}

std::string
CloudProjectsDatabase::GetProjectUserSlug(std::string_view projectId)
{
    auto connection = GetConnection();

    if (!connection) {
        return {}
    }

    auto statement = connection->CreateStatement(
        "SELECT user_name FROM project_users WHERE project_id = ? LIMIT 1");

    if (!statement) {
        return {}
    }

    auto result = statement->Prepare(projectId).Run();

    for (auto row : result) {
        std::string slug;

        if (!row.Get(0, slug)) {
            return {}
        }

        return slug;
    }

    return {};
}

void CloudProjectsDatabase::SetProjectUserSlug(
    std::string_view projectId, std::string_view slug)
{
    auto connection = GetConnection();

    if (!connection) {
        return;
    }

    auto statement = connection->CreateStatement(
        "INSERT OR REPLACE INTO project_users (project_id, user_name) VALUES (?, ?)");

    if (!statement) {
        return;
    }

    statement->Prepare(projectId, slug).Run();
}

bool CloudProjectsDatabase::IsProjectBlockLocked(
    std::string_view projectId, int64_t blockId) const
{
    auto connection = GetConnection();

    if (!connection) {
        return false;
    }

    auto statement = connection->CreateStatement(
        "SELECT 1 FROM pending_project_blocks WHERE project_id = ? AND block_id = ? LIMIT 1");

    if (!statement) {
        return false;
    }

    auto result = statement->Prepare(projectId, blockId).Run();

    for (auto row : result) {
        return true;
    }

    return false;
}

void CloudProjectsDatabase::AddPendingSnapshot(
    const PendingSnapshotData& snapshotData)
{
    auto connection = GetConnection();

    if (!connection) {
        return;
    }

    auto statement = connection->CreateStatement(
        "INSERT OR REPLACE INTO pending_snapshots (project_id, snapshot_id, confirm_url) VALUES (?, ?, ?)");

    if (!statement) {
        return;
    }

    statement
    ->Prepare(
        snapshotData.ProjectId, snapshotData.SnapshotId,
        snapshotData.ConfirmUrl)
    .Run();
}

void CloudProjectsDatabase::RemovePendingSnapshot(
    std::string_view projectId, std::string_view snapshotId)
{
    auto connection = GetConnection();

    if (!connection) {
        return;
    }

    static const char* queries[] = {
        "DELETE FROM pending_snapshots WHERE project_id = ? AND snapshot_id = ?",
        "DELETE FROM pending_project_blobs WHERE project_id = ? AND snapshot_id = ?",
        "DELETE FROM pending_project_blocks WHERE project_id = ? AND snapshot_id = ?",
    };

    auto tx = connection->BeginTransaction("RemovePendingSnapshot");

    for (auto query : queries) {
        auto statement = connection->CreateStatement(query);

        if (!statement) {
            return;
        }

        if (!statement->Prepare(projectId, snapshotId).Run().IsOk()) {
            return;
        }
    }

    tx.Commit();
}

std::vector<PendingSnapshotData>
CloudProjectsDatabase::GetPendingSnapshots(std::string_view projectId) const
{
    auto connection = GetConnection();

    if (!connection) {
        return {}
    }

    auto statement = connection->CreateStatement(
        "SELECT project_id, snapshot_id, confirm_url FROM pending_snapshots WHERE project_id = ?");

    if (!statement) {
        return {}
    }

    auto result = statement->Prepare(projectId).Run();

    std::vector<PendingSnapshotData> snapshots;

    for (auto row : result) {
        PendingSnapshotData data;

        if (!row.Get(0, data.ProjectId)) {
            return {}
        }

        if (!row.Get(1, data.SnapshotId)) {
            return {}
        }

        if (!row.Get(2, data.ConfirmUrl)) {
            return {}
        }

        snapshots.push_back(data);
    }

    return snapshots;
}

void CloudProjectsDatabase::AddPendingProjectBlob(
    const PendingProjectBlobData& blobData)
{
    auto connection = GetConnection();

    if (!connection) {
        return;
    }

    auto statement = connection->CreateStatement(
        "INSERT OR REPLACE INTO pending_project_blobs (project_id, snapshot_id, upload_url, confirm_url, fail_url, blob) VALUES (?, ?, ?, ?, ?, ?)");

    if (!statement) {
        return;
    }

    statement->Prepare()
    .Bind(1, blobData.ProjectId)
    .Bind(2, blobData.SnapshotId)
    .Bind(3, blobData.UploadUrl)
    .Bind(4, blobData.ConfirmUrl)
    .Bind(5, blobData.FailUrl)
    .Bind(6, blobData.BlobData.data(), blobData.BlobData.size(), false)
    .Run();
}

void CloudProjectsDatabase::RemovePendingProjectBlob(
    std::string_view projectId, std::string_view snapshotId)
{
    auto connection = GetConnection();

    if (!connection) {
        return;
    }

    auto statement = connection->CreateStatement(
        "DELETE FROM pending_project_blobs WHERE project_id = ? AND snapshot_id = ?");

    if (!statement) {
        return;
    }

    statement->Prepare(projectId, snapshotId).Run();
}

std::optional<PendingProjectBlobData>
CloudProjectsDatabase::GetPendingProjectBlob(
    std::string_view projectId, std::string_view snapshotId) const
{
    auto connection = GetConnection();

    if (!connection) {
        return {}
    }

    auto statement = connection->CreateStatement(
        "SELECT project_id, snapshot_id, upload_url, confirm_url, fail_url, blob FROM pending_project_blobs WHERE project_id = ? AND snapshot_id = ?");

    if (!statement) {
        return {}
    }

    auto result = statement->Prepare(projectId, snapshotId).Run();

    for (auto row : result) {
        PendingProjectBlobData data;

        if (!row.Get(0, data.ProjectId)) {
            return {}
        }

        if (!row.Get(1, data.SnapshotId)) {
            return {}
        }

        if (!row.Get(2, data.UploadUrl)) {
            return {}
        }

        if (!row.Get(3, data.ConfirmUrl)) {
            return {}
        }

        if (!row.Get(4, data.FailUrl)) {
            return {}
        }

        const auto size = row.GetColumnBytes(6);
        data.BlobData.resize(size);

        if (size != row.ReadData(6, data.BlobData.data(), size)) {
            return {}
        }

        return data;
    }

    return {};
}

void CloudProjectsDatabase::AddPendingProjectBlocks(
    const std::vector<PendingProjectBlockData>& blockData)
{
    auto connection = GetConnection();

    if (!connection) {
        return;
    }

    auto tx = connection->BeginTransaction("AddPendingProjectBlocks");

    auto statement = connection->CreateStatement(
        "INSERT OR REPLACE INTO pending_project_blocks (project_id, snapshot_id, upload_url, confirm_url, fail_url, block_id, block_sample_format, block_hash) VALUES (?, ?, ?, ?, ?, ?, ?, ?)");

    if (!statement) {
        return;
    }

    for (const auto& data : blockData) {
        statement->Prepare()
        .Bind(1, data.ProjectId)
        .Bind(2, data.SnapshotId)
        .Bind(3, data.UploadUrl)
        .Bind(4, data.ConfirmUrl)
        .Bind(5, data.FailUrl)
        .Bind(6, data.BlockId)
        .Bind(7, data.BlockSampleFormat)
        .Bind(8, data.BlockHash)
        .Run();
    }

    tx.Commit();
}

void CloudProjectsDatabase::RemovePendingProjectBlock(
    std::string_view projectId, int64_t blockId)
{
    auto connection = GetConnection();

    if (!connection) {
        return;
    }

    auto statement = connection->CreateStatement(
        "DELETE FROM pending_project_blocks WHERE project_id = ? AND block_id = ?");

    if (!statement) {
        return;
    }

    statement->Prepare(projectId, blockId).Run();
}

void CloudProjectsDatabase::RemovePendingProjectBlocks(
    std::string_view projectId, std::string_view snapshotId)
{
    auto connection = GetConnection();

    if (!connection) {
        return;
    }

    auto statement = connection->CreateStatement(
        "DELETE FROM pending_project_blocks WHERE project_id = ? AND snapshot_id = ?");

    if (!statement) {
        return;
    }

    statement->Prepare(projectId, snapshotId).Run();
}

std::vector<PendingProjectBlockData>
CloudProjectsDatabase::GetPendingProjectBlocks(
    std::string_view projectId, std::string_view snapshotId)
{
    auto connection = GetConnection();

    if (!connection) {
        return {}
    }

    auto statement = connection->CreateStatement(
        "SELECT project_id, snapshot_id, upload_url, confirm_url, fail_url, block_id, block_sample_format, block_hash FROM pending_project_blocks WHERE project_id = ? AND snapshot_id = ?");

    if (!statement) {
        return {}
    }

    auto result = statement->Prepare(projectId, snapshotId).Run();

    std::vector<PendingProjectBlockData> blocks;

    for (auto row : result) {
        PendingProjectBlockData data;

        if (!row.Get(0, data.ProjectId)) {
            return {}
        }

        if (!row.Get(1, data.SnapshotId)) {
            return {}
        }

        if (!row.Get(2, data.UploadUrl)) {
            return {}
        }

        if (!row.Get(3, data.ConfirmUrl)) {
            return {}
        }

        if (!row.Get(4, data.FailUrl)) {
            return {}
        }

        if (!row.Get(5, data.BlockId)) {
            return {}
        }

        if (!row.Get(6, data.BlockSampleFormat)) {
            return {}
        }

        if (!row.Get(7, data.BlockHash)) {
            return {}
        }

        blocks.push_back(data);
    }

    return blocks;
}

std::optional<DBProjectData>
CloudProjectsDatabase::DoGetProjectData(const sqlite::Row& row) const
{
    DBProjectData data;

    if (!row.Get(0, data.ProjectId)) {
        return {}
    }

    if (!row.Get(1, data.SnapshotId)) {
        return {}
    }

    if (!row.Get(2, data.SavesCount)) {
        return {}
    }

    if (!row.Get(3, data.LastAudioPreview)) {
        return {}
    }

    if (!row.Get(4, data.LocalPath)) {
        return {}
    }

    if (!row.Get(5, data.LastModified)) {
        return {}
    }

    if (!row.Get(6, data.LastRead)) {
        return {}
    }

    int status;
    if (!row.Get(7, status)) {
        return {}
    }

    data.SyncStatus = static_cast<DBProjectData::SyncStatusType>(status);

    if (!row.Get(8, data.FirstSyncDialogShown)) {
        return {}
    }

    return data;
}

std::optional<DBProjectData>
cloud::audiocom::sync::CloudProjectsDatabase::DoGetProjectData(
    sqlite::RunResult result) const
{
    for (auto row : result) {
        if (auto data = DoGetProjectData(row)) {
            return data;
        }
    }

    return {};
}

bool CloudProjectsDatabase::OpenConnection()
{
    if (mConnection) {
        return true;
    }

    const auto configDir  = FileNames::ConfigDir();
    const auto configPath = configDir + "/audiocom_sync.db";

    mConnection = sqlite::SafeConnection::Open(audacity::ToUTF8(configPath));

    if (!mConnection) {
        return false;
    }

    auto connection = mConnection->Acquire();

    auto result = connection->Execute(createTableQuery);

    if (!result) {
        mConnection = {};
        return false;
    }

    return RunMigrations();
}

bool CloudProjectsDatabase::RunMigrations()
{
    auto connection = mConnection->Acquire();

    auto getMigrationVersion
        =connection->CreateStatement("SELECT version FROM migration LIMIT 1");

    if (!getMigrationVersion) {
        return false;
    }

    auto versionResult = getMigrationVersion->Prepare().Run();

    if (!versionResult.IsOk()) {
        return false;
    }

    int version = 0;

    for (auto row : versionResult) {
        if (!row.Get(0, version)) {
            return false;
        }
        break;
    }

    const auto migrationsCount = std::size(migrations);

    if (version >= migrationsCount) {
        return true;
    }

    auto tx = connection->BeginTransaction("RunMigrations");

    for (int i = version; i < migrationsCount; ++i) {
        auto result = connection->Execute(migrations[i]);

        if (!result) {
            return false;
        }
    }

    auto updateVersion
        =connection->CreateStatement("UPDATE migration SET version = ?");

    if (!updateVersion) {
        return false;
    }

    if (!updateVersion->Prepare(migrationsCount).Run().IsOk()) {
        return false;
    }

    return tx.Commit().IsOk();
}

bool cloud::audiocom::sync::CloudProjectsDatabase::DeleteProject(
    sqlite::SafeConnection::Lock& connection, std::string_view projectId)
{
    static const char* queries[] = {
        "DELETE FROM projects WHERE project_id = ?",
        "DELETE FROM block_hashes WHERE project_id = ?",
        "DELETE FROM pending_snapshots WHERE project_id = ?",
        "DELETE FROM pending_project_blobs WHERE project_id = ?",
        "DELETE FROM pending_project_blocks WHERE project_id = ?",
        "DELETE FROM project_users WHERE project_id = ?",
    };

    for (auto query : queries) {
        auto statement = connection->CreateStatement(query);

        if (!statement) {
            return false;
        }

        if (!statement->Prepare(projectId).Run().IsOk()) {
            return false;
        }
    }

    return true;
}
} // namespace audacity::cloud::audiocom::sync
