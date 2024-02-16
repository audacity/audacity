/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  RemoteProjectSnapshot.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <atomic>
#include <condition_variable>
#include <functional>
#include <memory>
#include <mutex>
#include <string>
#include <thread>
#include <unordered_set>

#include "CloudSyncUtils.h"
#include "NetworkUtils.h"

namespace audacity::network_manager
{
class IResponse;
using ResponsePtr = std::shared_ptr<IResponse>;
}

namespace cloud::audiocom::sync
{

struct RemoteProjectSnapshotState final
{
   ResponseResult Result;

   int64_t BlocksDownloaded { 0 };
   int64_t BlocksTotal { 0 };

   bool ProjectDownloaded { false };

   bool IsComplete() const noexcept;
};

using RemoteProjectSnapshotStateCallback =
   std::function<void(RemoteProjectSnapshotState)>;

class RemoteProjectSnapshot final
{
   struct Tag
   {
   };

public:
   RemoteProjectSnapshot(
      Tag, ProjectInfo projectInfo, SnapshotInfo snapshotInfo, std::string path,
      RemoteProjectSnapshotStateCallback callback);

   ~RemoteProjectSnapshot();

   RemoteProjectSnapshot(const RemoteProjectSnapshot&)            = delete;
   RemoteProjectSnapshot& operator=(const RemoteProjectSnapshot&) = delete;
   RemoteProjectSnapshot(RemoteProjectSnapshot&&)                 = delete;
   RemoteProjectSnapshot& operator=(RemoteProjectSnapshot&&)      = delete;

   static std::shared_ptr<RemoteProjectSnapshot> Sync(
      ProjectInfo projectInfo, SnapshotInfo snapshotInfo, std::string path,
      RemoteProjectSnapshotStateCallback callback);

   void Cancel();

private:
   

   using SuccessHandler = std::function<void(audacity::network_manager::ResponsePtr)>;
   std::unordered_set<std::string> CalculateKnownBlocks() const;

   void DoCancel();

   void
   DownloadBlob(std::string url, SuccessHandler onSuccess, int retries = 3);

   void OnProjectBlobDownloaded(audacity::network_manager::ResponsePtr response);
   void OnBlockDownloaded(
      std::string blockHash, audacity::network_manager::ResponsePtr response);

   void OnFailure(ResponseResult result);
   void RemoveResponse(audacity::network_manager::IResponse* response);

   void MarkProjectInDB(bool successfulDownload);

   void ReportProgress();

   bool WantsNextRequest() const;
   void RequestsThread();

   const std::string mSnapshotDBName;
   const ProjectInfo mProjectInfo;
   const SnapshotInfo mSnapshotInfo;
   const std::string mPath;
   RemoteProjectSnapshotStateCallback mCallback;

   std::thread mRequestsThread;
   std::mutex mRequestsMutex;
   std::condition_variable mRequestsCV;

   std::vector<std::pair<std::string, SuccessHandler>> mRequests;

   int mRequestsInProgress { 0 };
   size_t mNextRequestIndex { 0 };

   std::mutex mResponsesMutex;
   std::vector<std::shared_ptr<audacity::network_manager::IResponse>>
      mResponses;
   std::condition_variable mResponsesEmptyCV;

   std::atomic<int64_t> mDownloadedBlocks { 0 };
   int64_t mMissingBlocks { 0 };

   std::atomic<bool> mCancelled { false };
   std::atomic<bool> mFailed { false };
   std::atomic<bool> mProjectDownloaded { false };

   bool mAttached { false };
   bool mNothingToDo { false };
   
}; // class RemoteProjectSnapshot
} // namespace cloud::audiocom::sync
