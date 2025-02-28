/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectsDatabase.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <mutex>
#include <optional>
#include <string>
#include <vector>

#include "sqlite/SafeConnection.h"

#include "CloudSyncDTO.h"

namespace audacity::cloud::audiocom::sync {
struct DBProjectData final
{
    std::string ProjectId;
    std::string SnapshotId;
    int64_t SavesCount       = 0;
    int64_t LastAudioPreview = 0;
    std::string LocalPath;
    int64_t LastModified = 0;
    int64_t LastRead     = 0;

    bool FirstSyncDialogShown { false };

    enum SyncStatusType
    {
        SyncStatusSynced      = 0,
        SyncStatusUploading   = 1,
        SyncStatusDownloading = 2,
    } SyncStatus = {};
};

struct PendingSnapshotData final
{
    std::string ProjectId;
    std::string SnapshotId;
    std::string ConfirmUrl;
};

struct PendingProjectBlobData final
{
    std::string ProjectId;
    std::string SnapshotId;

    std::string UploadUrl;
    std::string ConfirmUrl;
    std::string FailUrl;

    std::vector<uint8_t> BlobData;
};

struct PendingProjectBlockData final
{
    std::string ProjectId;
    std::string SnapshotId;

    std::string UploadUrl;
    std::string ConfirmUrl;
    std::string FailUrl;

    int64_t BlockId {};
    int BlockSampleFormat {};

    std::string BlockHash;
};

class CloudProjectsDatabase final
{
    CloudProjectsDatabase() = default;
    ~CloudProjectsDatabase() = default;

public:
    static CloudProjectsDatabase& Get();

    sqlite::SafeConnection::Lock GetConnection();
    const sqlite::SafeConnection::Lock GetConnection() const;

    std::optional<DBProjectData>
    GetProjectData(std::string_view projectId) const;
    std::optional<DBProjectData>
    GetProjectDataForPath(const std::string& projectPath) const;

    std::vector<DBProjectData> GetCloudProjects() const;

    void DeleteProject(std::string_view projectId);

    bool
    MarkProjectAsSynced(std::string_view projectId, std::string_view snapshotId);

    void UpdateProjectBlockList(
        std::string_view projectId, const SampleBlockIDSet& blockSet);

    std::optional<std::string>
    GetBlockHash(std::string_view projectId, int64_t blockId) const;

    void UpdateBlockHashes(
        std::string_view projectId, const std::vector<std::pair<int64_t, std::string> >& hashes);

    bool UpdateProjectData(const DBProjectData& projectData);

    bool IsFirstSyncDialogShown(std::string_view projectId) const;
    void SetFirstSyncDialogShown(std::string_view projectId, bool shown = true);

    std::string GetProjectUserSlug(std::string_view projectId);
    void SetProjectUserSlug(std::string_view projectId, std::string_view slug);

    bool IsProjectBlockLocked(std::string_view projectId, int64_t blockId) const;

    void AddPendingSnapshot(const PendingSnapshotData& snapshotData);
    void RemovePendingSnapshot(
        std::string_view projectId, std::string_view snapshotId);
    std::vector<PendingSnapshotData>
    GetPendingSnapshots(std::string_view projectId) const;

    void AddPendingProjectBlob(const PendingProjectBlobData& blobData);
    void RemovePendingProjectBlob(
        std::string_view projectId, std::string_view snapshotId);
    std::optional<PendingProjectBlobData> GetPendingProjectBlob(
        std::string_view projectId, std::string_view snapshotId) const;

    void AddPendingProjectBlocks(
        const std::vector<PendingProjectBlockData>& blockData);
    void RemovePendingProjectBlock(
        std::string_view projectId, int64_t blockId);
    void RemovePendingProjectBlocks(
        std::string_view projectId, std::string_view snapshotId);
    std::vector<PendingProjectBlockData> GetPendingProjectBlocks(
        std::string_view projectId, std::string_view snapshotId);

private:
    std::optional<DBProjectData>
    DoGetProjectData(const sqlite::Row& result) const;

    std::optional<DBProjectData>
    DoGetProjectData(sqlite::RunResult result) const;

    bool OpenConnection();
    bool RunMigrations();

    bool DeleteProject(sqlite::SafeConnection::Lock& connection, std::string_view projectId);

    std::mutex mConnectionMutex;
    std::shared_ptr<sqlite::SafeConnection> mConnection;
};
} // namespace audacity::cloud::audiocom::sync
