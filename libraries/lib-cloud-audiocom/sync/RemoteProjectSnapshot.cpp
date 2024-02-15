/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  RemoteProjectSnapshot.cpp

  Dmitry Vedenko

**********************************************************************/
#include "RemoteProjectSnapshot.h"

#include <algorithm>

#include <wx/datetime.h>

#include "CloudProjectsDatabase.h"

#include "CodeConversions.h"
#include "Internat.h"
#include "MemoryX.h"
#include "StringUtils.h"

#include "IResponse.h"
#include "NetworkManager.h"
#include "Request.h"

#include "WavPackCompressor.h"

namespace cloud::audiocom::sync
{
RemoteProjectSnapshot::RemoteProjectSnapshot(
   Tag, ProjectInfo projectInfo, SnapshotInfo snapshotInfo, std::string path,
   RemoteProjectSnapshotStateCallback callback)
    : mSnapshotDBName { std::string("s_") + projectInfo.Id }
    , mProjectInfo { std::move(projectInfo) }
    , mSnapshotInfo { std::move(snapshotInfo) }
    , mPath { std::move(path) }
    , mCallback { std::move(callback) }
{
   {
      auto writeLock  = std::lock_guard { mDbWriteMutex };
      auto& db        = CloudProjectsDatabase::Get().GetConnection();
      auto attachStmt = db.CreateStatement("ATTACH DATABASE ? AS ?");
      auto result     = attachStmt->Prepare(mPath, mSnapshotDBName).Run();
      mAttached       = result.IsOk();

      if (!mAttached)
         return;
   }

   auto knownBlocks = CalculateKnownBlocks();

   if (knownBlocks.size() == mSnapshotInfo.Blocks.size())
   {
      auto syncInfo =
         CloudProjectsDatabase::Get().GetProjectData(mProjectInfo.Id);

      if (
         syncInfo && syncInfo->SnapshotId == mSnapshotInfo.Id &&
         syncInfo->SyncStatus == DBProjectData::SyncStatusSynced)
      {
         mCallback({ {}, 0, 0, true });
         mNothingToDo = true;
         return;
      }
   }

   {
      auto writeLock = std::lock_guard { mDbWriteMutex };
      MarkProjectInDB(false);
   }

   mMissingBlocks = mSnapshotInfo.Blocks.size() - knownBlocks.size();
   mRequests.reserve(1 + mMissingBlocks);

   mRequests.push_back(std::make_pair(
      mSnapshotInfo.FileUrl,
      [this](auto response) { OnProjectBlobDownloaded(response); }));

   for (auto& block : mSnapshotInfo.Blocks)
   {
      if (knownBlocks.find(ToUpper(block.Hash)) != knownBlocks.end())
         continue;

      mRequests.push_back(std::make_pair(
         block.Url, [this, hash = ToUpper(block.Hash)](auto response)
         { OnBlockDownloaded(std::move(hash), response); }));
   }

   mRequestsThread =
      std::thread { &RemoteProjectSnapshot::RequestsThread, this };
}

RemoteProjectSnapshot::~RemoteProjectSnapshot()
{
   DoCancel();

   if (mRequestsThread.joinable())
      mRequestsThread.join();

   {
      auto lock = std::unique_lock { mResponsesMutex };
      mResponsesEmptyCV.wait(lock, [this] { return mResponses.empty(); });
   }

   if (mAttached)
   {
      auto writeLock  = std::lock_guard { mDbWriteMutex };
      auto& db        = CloudProjectsDatabase::Get().GetConnection();
      auto detachStmt = db.CreateStatement("DETACH DATABASE ?");
      detachStmt->Prepare(mSnapshotDBName).Run();
   }
}

std::shared_ptr<RemoteProjectSnapshot> RemoteProjectSnapshot::Sync(
   ProjectInfo projectInfo, SnapshotInfo snapshotInfo, std::string path,
   RemoteProjectSnapshotStateCallback callback)
{
   auto snapshot = std::make_shared<RemoteProjectSnapshot>(
      Tag {}, std::move(projectInfo), std::move(snapshotInfo), std::move(path),
      std::move(callback));

   if (!snapshot->mAttached)
   {
      snapshot->mCallback(
         { { ResponseResultCode::InternalClientError,
             audacity::ToUTF8(
                XO("Failed to attach to the cloud project database")
                   .Translation()) },
           0,
           0,
           false });

      return {};
   }

   if (snapshot->mNothingToDo)
      return {};

   return snapshot;
}

void RemoteProjectSnapshot::Cancel()
{
   DoCancel();

   mCallback({ { ResponseResultCode::Cancelled, {} },
               mDownloadedBlocks.load(),
               mMissingBlocks,
               mProjectDownloaded.load() });
}

std::unordered_set<std::string>
RemoteProjectSnapshot::CalculateKnownBlocks() const
{
   std::unordered_set<std::string> remoteBlocks;

   for (const auto& block : mSnapshotInfo.Blocks)
      remoteBlocks.insert(ToUpper(block.Hash));

   auto& db = CloudProjectsDatabase::Get().GetConnection();

   auto fn = db.CreateScalarFunction(
      "inRemoteBlocks", [&remoteBlocks](const std::string& hash)
      { return remoteBlocks.find(hash) != remoteBlocks.end(); });

   auto statement = db.CreateStatement(
      "SELECT hash FROM block_hashes WHERE project_id = ? AND inRemoteBlocks(hash) AND block_id IN (SELECT blockid FROM " +
      mSnapshotDBName + ".sampleblocks)");

   if (!statement)
      return {};

   auto result = statement->Prepare(mProjectInfo.Id).Run();

   std::unordered_set<std::string> knownBlocks;

   for (auto row : result)
   {
      std::string hash;

      if (!row.Get(0, hash))
         continue;

      knownBlocks.insert(hash);
   }

   return knownBlocks;
}

void RemoteProjectSnapshot::DoCancel()
{
   mCancelled.store(true, std::memory_order_release);
   mRequestsCV.notify_one();

   {
      auto responsesLock = std::lock_guard { mResponsesMutex };
      for (auto& response : mResponses)
         response->abort();
   }
}

void RemoteProjectSnapshot::DownloadBlob(
   std::string url, SuccessHandler onSuccess, int retries)
{
   using namespace audacity::network_manager;

   auto request = Request(url);

   auto response = NetworkManager::GetInstance().doGet(request);

   {
      auto responsesLock = std::lock_guard { mResponsesMutex };
      mResponses.push_back(response);
   }

   response->setRequestFinishedCallback(
      [this, onSuccess = std::move(onSuccess), retries, response](auto)
      {
         RemoveResponse(response.get());

         auto responseResult = GetResponseResult(*response, false);

         if (responseResult.Code == ResponseResultCode::Cancelled)
            return;

         if (
            responseResult.Code != ResponseResultCode::Success &&
            responseResult.Code != ResponseResultCode::ConnectionFailed)
         {
            OnFailure(std::move(responseResult));
            return;
         }

         if (responseResult.Code == ResponseResultCode::ConnectionFailed)
         {
            if (retries <= 0)
            {
               OnFailure(std::move(responseResult));
               return;
            }

            DownloadBlob(
               response->getRequest().getURL(), std::move(onSuccess),
               retries - 1);

            return;
         }

         onSuccess(response);
      });
}

void RemoteProjectSnapshot::OnProjectBlobDownloaded(
   audacity::network_manager::ResponsePtr response)
{
   const std::vector<uint8_t> data = response->readAll();
   uint64_t dictSize               = 0;

   if (data.size() < sizeof(uint64_t))
   {
      OnFailure({ ResponseResultCode::UnexpectedResponse, {} });
      return;
   }

   std::memcpy(&dictSize, data.data(), sizeof(uint64_t));

   if (!IsLittleEndian())
      dictSize = SwapIntBytes(dictSize);

   if (data.size() < sizeof(uint64_t) + dictSize)
   {
      OnFailure({ ResponseResultCode::UnexpectedResponse, {} });
      return;
   }
   {
      auto writeLock = std::lock_guard { mDbWriteMutex };

      auto& db         = CloudProjectsDatabase::Get().GetConnection();
      auto transaction = db.BeginTransaction("p_" + mProjectInfo.Id);

      auto updateProjectStatement = db.CreateStatement(
         "INSERT INTO " + mSnapshotDBName +
         ".project (id, dict, doc) VALUES (1, ?1, ?2) "
         "ON CONFLICT(id) DO UPDATE SET dict = ?1, doc = ?2");

      if (!updateProjectStatement)
      {
         OnFailure({ ResponseResultCode::InternalClientError,
                     audacity::ToUTF8(updateProjectStatement.GetError()
                                         .GetErrorString()
                                         .Translation()) });
         return;
      }

      auto& preparedUpdateProjectStatement = updateProjectStatement->Prepare();

      preparedUpdateProjectStatement.Bind(
         1, data.data() + sizeof(uint64_t), dictSize, false);

      preparedUpdateProjectStatement.Bind(
         2, data.data() + sizeof(uint64_t) + dictSize,
         data.size() - sizeof(uint64_t) - dictSize, false);

      auto result = preparedUpdateProjectStatement.Run();

      if (!result.IsOk())
      {
         OnFailure(
            { ResponseResultCode::InternalClientError,
              audacity::ToUTF8(
                 result.GetErrors().front().GetErrorString().Translation()) });

         return;
      }

      auto deleteAutosaveStatement = db.CreateStatement(
         "DELETE FROM " + mSnapshotDBName + ".autosave WHERE id = 1");

      if (!deleteAutosaveStatement)
      {
         OnFailure({ ResponseResultCode::InternalClientError,
                     audacity::ToUTF8(deleteAutosaveStatement.GetError()
                                         .GetErrorString()
                                         .Translation()) });
         return;
      }

      result = deleteAutosaveStatement->Prepare().Run();

      if (!result.IsOk())
      {
         OnFailure(
            { ResponseResultCode::InternalClientError,
              audacity::ToUTF8(
                 result.GetErrors().front().GetErrorString().Translation()) });
         return;
      }

      if (auto error = transaction.Commit(); error.IsError())
      {
         OnFailure({ ResponseResultCode::InternalClientError,
                     audacity::ToUTF8(error.GetErrorString().Translation()) });
         return;
      }
   }

   mProjectDownloaded.store(true, std::memory_order_release);
   ReportProgress();
}

void RemoteProjectSnapshot::OnBlockDownloaded(
   std::string blockHash, audacity::network_manager::ResponsePtr response)
{
   const auto compressedData = response->readAll<std::vector<uint8_t>>();
   const auto blockData =
      DecompressBlock(compressedData.data(), compressedData.size());

   if (!blockData)
   {
      OnFailure(
         { ResponseResultCode::InternalClientError,
           audacity::ToUTF8(XO("Failed to decompress the cloud project block")
                               .Translation()) });
      return;
   }
   {
      auto writeLock = std::lock_guard { mDbWriteMutex };

      auto& db         = CloudProjectsDatabase::Get().GetConnection();
      auto transaction = db.BeginTransaction("b_" + blockHash);

      auto hashesStatement = db.CreateStatement(
         "INSERT INTO block_hashes (project_id, block_id, hash) VALUES (?1, ?2, ?3) "
         "ON CONFLICT(project_id, block_id) DO UPDATE SET hash = ?3");

      auto result = hashesStatement
                       ->Prepare(mProjectInfo.Id, blockData->BlockId, blockHash)
                       .Run();

      if (!result.IsOk())
      {
         OnFailure(
            { ResponseResultCode::InternalClientError,
              audacity::ToUTF8(
                 result.GetErrors().front().GetErrorString().Translation()) });
         return;
      }

      auto blockStatement = db.CreateStatement(
         "INSERT INTO " + mSnapshotDBName +
         ".sampleblocks (blockid, sampleformat, summin, summax, sumrms, summary256, summary64k, samples) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8) "
         "ON CONFLICT(blockid) DO UPDATE SET sampleformat = ?2, summin = ?3, summax = ?4, sumrms = ?5, summary256 = ?6, summary64k = ?7, samples = ?8");

      if (!blockStatement)
      {
         OnFailure(
            { ResponseResultCode::InternalClientError,
              audacity::ToUTF8(
                 blockStatement.GetError().GetErrorString().Translation()) });
         return;
      }

      auto& preparedStatement = blockStatement->Prepare();

      preparedStatement.Bind(1, blockData->BlockId);
      preparedStatement.Bind(2, static_cast<int64_t>(blockData->Format));
      preparedStatement.Bind(3, blockData->BlockMinMaxRMS.Min);
      preparedStatement.Bind(4, blockData->BlockMinMaxRMS.Max);
      preparedStatement.Bind(5, blockData->BlockMinMaxRMS.RMS);
      preparedStatement.Bind(
         6, blockData->Summary256.data(),
         blockData->Summary256.size() * sizeof(MinMaxRMS), false);
      preparedStatement.Bind(
         7, blockData->Summary64k.data(),
         blockData->Summary64k.size() * sizeof(MinMaxRMS), false);
      preparedStatement.Bind(
         8, blockData->Data.data(), blockData->Data.size(), false);

      result = preparedStatement.Run();

      if (!result.IsOk())
      {
         OnFailure(
            { ResponseResultCode::InternalClientError,
              audacity::ToUTF8(
                 result.GetErrors().front().GetErrorString().Translation()) });
         return;
      }

      if (auto error = transaction.Commit(); error.IsError())
      {
         OnFailure({ ResponseResultCode::InternalClientError,
                     audacity::ToUTF8(error.GetErrorString().Translation()) });
         return;
      }
   }

   mDownloadedBlocks.fetch_add(1, std::memory_order_acq_rel);

   ReportProgress();
}

void RemoteProjectSnapshot::OnFailure(ResponseResult result)
{
   mFailed.store(true, std::memory_order_release);
   mCallback({ result, mDownloadedBlocks.load(std::memory_order_acquire),
               mMissingBlocks,
               mProjectDownloaded.load(std::memory_order_acquire) });
}

void RemoteProjectSnapshot::RemoveResponse(
   audacity::network_manager::IResponse* response)
{
   {
      auto lock = std::lock_guard { mResponsesMutex };
      mResponses.erase(
         std::remove_if(
            mResponses.begin(), mResponses.end(),
            [response](auto& r) { return r.get() == response; }),
         mResponses.end());

      if (mResponses.empty())
         mRequestsCV.notify_all();
   }
   {
      auto lock = std::lock_guard { mRequestsMutex };
      mRequestsInProgress--;
      mRequestsCV.notify_one();
   }
}

void RemoteProjectSnapshot::MarkProjectInDB(bool successfulDownload)
{
   auto& db         = CloudProjectsDatabase::Get();
   auto currentData = db.GetProjectData(mProjectInfo.Id);

   auto data = currentData ? *currentData : DBProjectData {};

   data.ProjectId  = mProjectInfo.Id;
   data.SnapshotId = mSnapshotInfo.Id;
   data.SyncStatus = successfulDownload ? DBProjectData::SyncStatusSynced :
                                          DBProjectData::SyncStatusDownloading;
   data.LastRead   = wxDateTime::Now().GetTicks();
   data.LocalPath  = mPath;

   db.UpdateProjectData(data);
}

void RemoteProjectSnapshot::ReportProgress()
{
   if (mCancelled.load(std::memory_order_acquire))
      return;

   const auto projectDownloaded =
      mProjectDownloaded.load(std::memory_order_acquire);
   const auto blocksDownloaded =
      mDownloadedBlocks.load(std::memory_order_acquire);

   const auto completed =
      blocksDownloaded == mMissingBlocks && projectDownloaded;

   // This happens under the write lock
   if (completed)
   {
      auto writeLock = std::lock_guard { mDbWriteMutex };
      MarkProjectInDB(true);
   }

   mCallback({ {}, blocksDownloaded, mMissingBlocks, projectDownloaded });
}

bool RemoteProjectSnapshot::WantsNextRequest() const
{
   return !mCancelled.load(std::memory_order_acquire) &&
          !mFailed.load(std::memory_order_acquire);
}

void RemoteProjectSnapshot::RequestsThread()
{
   constexpr auto MAX_CONCURRENT_REQUESTS = 6;

   while (WantsNextRequest())
   {
      std::pair<std::string, SuccessHandler> request;

      {
         auto lock = std::unique_lock { mRequestsMutex };

         if (mRequestsInProgress >= MAX_CONCURRENT_REQUESTS)
         {
            mRequestsCV.wait(
               lock,
               [this, MAX_CONCURRENT_REQUESTS]
               {
                  return mRequestsInProgress < MAX_CONCURRENT_REQUESTS ||
                         !WantsNextRequest();
               });
         }

         if (!WantsNextRequest())
            return;

         if (mNextRequestIndex >= mRequests.size())
            return;

         request = mRequests[mNextRequestIndex++];
         mRequestsInProgress++;
      }

      DownloadBlob(std::move(request.first), std::move(request.second), 3);

      // TODO: Random sleep to avoid overloading the server
      std::this_thread::sleep_for(std::chrono::milliseconds(50));
   }
}

bool RemoteProjectSnapshotState::IsComplete() const noexcept
{
   return (BlocksDownloaded == BlocksTotal && ProjectDownloaded) ||
          Result.Code != ResponseResultCode::Success;
}

} // namespace cloud::audiocom::sync
