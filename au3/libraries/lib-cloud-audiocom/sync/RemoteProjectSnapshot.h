/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  RemoteProjectSnapshot.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <atomic>
#include <chrono>
#include <condition_variable>
#include <future>
#include <functional>
#include <memory>
#include <mutex>
#include <string>
#include <thread>
#include <unordered_set>

#include "CloudSyncDTO.h"
#include "NetworkUtils.h"

namespace audacity::network_manager {
class IResponse;
using ResponsePtr = std::shared_ptr<IResponse>;
} // namespace audacity::network_manager

namespace audacity::cloud::audiocom::sync {
struct RemoteProjectSnapshotState final
{
    ResponseResult Result;

    int64_t BlocksDownloaded { 0 };
    int64_t BlocksTotal { 0 };

    bool ProjectDownloaded { false };

    bool IsComplete() const noexcept;
};

using RemoteProjectSnapshotStateCallback
    =std::function<void (RemoteProjectSnapshotState)>;

class RemoteProjectSnapshot final : public std::enable_shared_from_this<RemoteProjectSnapshot>
{
    struct Tag
    {
    };

public:
    RemoteProjectSnapshot(
        Tag, ProjectInfo projectInfo, SnapshotInfo snapshotInfo, std::string path, RemoteProjectSnapshotStateCallback callback,
        bool downloadDetached);

    ~RemoteProjectSnapshot();

    RemoteProjectSnapshot(const RemoteProjectSnapshot&)            = delete;
    RemoteProjectSnapshot& operator=(const RemoteProjectSnapshot&) = delete;
    RemoteProjectSnapshot(RemoteProjectSnapshot&&)                 = delete;
    RemoteProjectSnapshot& operator=(RemoteProjectSnapshot&&)      = delete;

    static std::shared_ptr<RemoteProjectSnapshot> Sync(
        ProjectInfo projectInfo, SnapshotInfo snapshotInfo, std::string path, RemoteProjectSnapshotStateCallback callback,
        bool downloadDetached);

    void Cancel();

    TransferStats GetTransferStats() const;

    std::string_view GetProjectId() const;

private:
    enum class State
    {
        Downloading,
        Cancelled,
        Failed,
        Succeeded
    };

    using SuccessHandler
        =std::function<void (audacity::network_manager::ResponsePtr)>;

    std::string AttachOriginalDB();

    void SetupBlocksCopy(
        const std::string& dbName, std::unordered_set<std::string> blocks);

    std::unordered_set<std::string>
    CalculateKnownBlocks(const std::string& attachedDbName) const;

    void DoCancel();

    void
    DownloadBlob(std::string url, SuccessHandler onSuccess, int retries = 3);

    void
    OnProjectBlobDownloaded(audacity::network_manager::ResponsePtr response);
    void OnBlockDownloaded(
        std::string blockHash, audacity::network_manager::ResponsePtr response);

    void OnFailure(ResponseResult result);
    void RemoveResponse(audacity::network_manager::IResponse* response);

    void MarkProjectInDB(bool successfulDownload);

    void ReportProgress();

    bool InProgress() const;
    void RequestsThread();

    void SetState(State state);

    void CleanupOrphanBlocks();

    const std::string mSnapshotDBName;
    const ProjectInfo mProjectInfo;
    const SnapshotInfo mSnapshotInfo;
    const std::string mPath;
    RemoteProjectSnapshotStateCallback mCallback;

    std::vector<std::string> mAttachedDBNames;

    std::atomic<State> mState { State::Downloading };

    using Clock     = std::chrono::steady_clock;
    using TimePoint = Clock::time_point;

    TimePoint mStartTime { Clock::now() };
    TimePoint mEndTime;

    std::thread mRequestsThread;
    std::mutex mRequestsMutex;
    std::condition_variable mRequestsCV;

    std::vector<std::pair<std::string, SuccessHandler> > mRequests;

    int mRequestsInProgress { 0 };
    size_t mNextRequestIndex { 0 };

    std::mutex mResponsesMutex;
    std::vector<std::shared_ptr<audacity::network_manager::IResponse> >
    mResponses;
    std::condition_variable mResponsesEmptyCV;

    std::atomic<int64_t> mDownloadedBlocks { 0 };
    std::atomic<int64_t> mCopiedBlocks { 0 };
    std::atomic<int64_t> mDownloadedBytes { 0 };

    int64_t mMissingBlocks { 0 };

    std::optional<std::future<bool> > mCopyBlocksFuture;

    std::atomic<bool> mProjectDownloaded { false };

    bool mNothingToDo { false };
    const bool mDownloadDetached { false };
}; // class RemoteProjectSnapshot
} // namespace audacity::cloud::audiocom::sync
