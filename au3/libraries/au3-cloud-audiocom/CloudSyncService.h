/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSyncService.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <atomic>
#include <functional>
#include <future>
#include <memory>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "NetworkUtils.h"

#include "au3-concurrency/concurrency/CancellationContext.h"

class AudacityProject;

namespace audacity::cloud::audiocom {
namespace sync {
class LocalProjectSnapshot;
class PaginatedProjectsResponse;
class PaginatedAudioResponse;
class CloudAudioInfo;
class ProjectInfo;
class SnapshotInfo;
class RemoteProjectSnapshot;

struct ProjectSyncResult final
{
    enum class StatusCode
    {
        Succeeded,
        Blocked,
        Failed,
    };

    StatusCode Status { StatusCode::Failed };
    ResponseResult Result;
    std::string ProjectPath;
    TransferStats Stats;
}; // struct ProjectSyncResult

struct DownloadAudioResult final
{
    enum class StatusCode
    {
        Succeeded,
        Failed,
        Blocked,
    };

    StatusCode Status { StatusCode::Failed };
    ResponseResult Result;
    std::string AudioPath;
    std::string Title;
    std::string Format;
}; // struct DownloadAudioResult

using ProgressCallback = std::function<bool (double)>;

using GetProjectsResult
    =std::variant<sync::PaginatedProjectsResponse, ResponseResult>;

using GetHeadSnapshotIDResult = std::variant<std::string, ResponseResult>;

using GetAudioListResult = std::variant<sync::PaginatedAudioResponse, ResponseResult>;
using GetAudioInfoResult = std::variant<sync::CloudAudioInfo, ResponseResult>;
} // namespace sync

//! CloudSyncService is responsible for saving and loading projects from the
//! cloud
class CLOUD_AUDIOCOM_API CloudSyncService final
{
    CloudSyncService()  = default;
    ~CloudSyncService() = default;

    CloudSyncService(const CloudSyncService&)            = delete;
    CloudSyncService(CloudSyncService&&)                 = delete;
    CloudSyncService& operator=(const CloudSyncService&) = delete;
    CloudSyncService& operator=(CloudSyncService&&)      = delete;

public:
    static CloudSyncService& Get();

    using SyncPromise = std::promise<sync::ProjectSyncResult>;
    using SyncFuture  = std::future<sync::ProjectSyncResult>;

    using GetProjectsPromise = std::promise<sync::GetProjectsResult>;
    using GetProjectsFuture  = std::future<sync::GetProjectsResult>;

    using GetHeadSnapshotIDPromise = std::promise<sync::GetHeadSnapshotIDResult>;
    using GetHeadSnapshotIDFuture  = std::future<sync::GetHeadSnapshotIDResult>;

    using GetAudioListPromise = std::promise<sync::GetAudioListResult>;
    using GetAudioListFuture  = std::future<sync::GetAudioListResult>;

    using GetAudioInfoPromise = std::promise<sync::GetAudioInfoResult>;
    using GetAudioInfoFuture  = std::future<sync::GetAudioInfoResult>;

    using DownloadAudioPromise = std::promise<sync::DownloadAudioResult>;
    using DownloadAudioFuture  = std::future<sync::DownloadAudioResult>;

    enum class SyncMode
    {
        Normal,
        ForceOverwrite,
        ForceNew
    };

    //! Retrieve the list of projects from the cloud
    GetProjectsFuture GetProjects(
        concurrency::CancellationContextPtr context, int page, int pageSize, std::string_view searchString);

    //! Retrieve the list of audio from the cloud
    GetAudioListFuture GetAudioList(
        concurrency::CancellationContextPtr context, int page, int pageSize, std::string_view searchString);

    [[nodiscard]] DownloadAudioFuture DownloadCloudAudio(
        std::string_view audioId, sync::ProgressCallback callback);

    //! Open the project from the cloud. This operation is asynchronous.
    [[nodiscard]] SyncFuture OpenFromCloud(
        std::string projectId, std::string snapshotId, SyncMode mode, sync::ProgressCallback callback);

    [[nodiscard]] SyncFuture SyncProject(
        AudacityProject& project, const std::string& path, bool forceSync, sync::ProgressCallback callback);

    static bool IsCloudProject(const std::string& path);

    enum class ProjectState
    {
        NotAvaliable,
        PendingSync,
        FullySynced
    };
    static ProjectState GetProjectState(const std::string& projectId);

    GetHeadSnapshotIDFuture GetHeadSnapshotID(std::string_view projectId);

private:
    void FailSync(ResponseResult responseResult);

    void CompleteSync(std::string path);

    void CompleteSync(sync::ProjectSyncResult result);

    void SyncCloudSnapshot(
        const sync::ProjectInfo& projectInfo, const sync::SnapshotInfo& snapshotInfo, SyncMode mode);

    void DownloadAudio(const std::string& name, const std::string& format, const std::string& url);

    void UpdateDowloadProgress(double downloadProgress);

    void
    ReportUploadStats(std::string_view projectId, const TransferStats& stats);

    std::vector<std::shared_ptr<sync::LocalProjectSnapshot> > mLocalSnapshots;
    std::shared_ptr<sync::RemoteProjectSnapshot> mRemoteSnapshot;

    SyncPromise mSyncPromise;
    sync::ProgressCallback mProgressCallback;

    DownloadAudioPromise mDownloadAudioPromise;

    std::atomic<double> mDownloadProgress { 0.0 };
    std::atomic<bool> mProgressUpdateQueued { false };
    std::atomic<bool> mAudioProgressUpdateQueued { false };

    std::atomic<bool> mSyncInProcess { false };
    std::atomic<bool> mDownloadInProcess { false };
};
} // namespace audacity::cloud::audiocom
