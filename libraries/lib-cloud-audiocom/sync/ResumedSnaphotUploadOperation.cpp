/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ResumedSnaphotUploadOperation.cpp

  Dmitry Vedenko

**********************************************************************/

#include "ResumedSnaphotUploadOperation.h"

#include <memory>
#include <string>
#include <string_view>

#include "concurrency/CancellationContext.h"

#include "CloudProjectsDatabase.h"
#include "DataUploader.h"
#include "MissingBlocksUploader.h"
#include "ProjectCloudExtension.h"
#include "ProjectUploadOperation.h"
#include "ServiceConfig.h"

#include "SampleBlock.h"
#include "WaveTrack.h"

#include "IResponse.h"
#include "NetworkManager.h"
#include "Request.h"

namespace audacity::cloud::audiocom::sync
{
namespace
{
class ResumedSnaphotUploadOperation final :
    public ProjectUploadOperation,
    public std::enable_shared_from_this<ResumedSnaphotUploadOperation>
{
   struct Tag
   {
   };

public:
   ResumedSnaphotUploadOperation(
      Tag, ProjectCloudExtension& projectCloudExtension,
      std::string_view snapshotId, std::string_view confirmationUrl)
       : mProjectCloudExtension { projectCloudExtension }
       , mProjectId { mProjectCloudExtension.GetCloudProjectId() }
       , mSnapshotId { snapshotId }
       , mConfirmationUrl { confirmationUrl }
       , mCancellationContext { concurrency::CancellationContext::Create() }
   {
   }

   ~ResumedSnaphotUploadOperation() override
   {
   }

   static void Perform(
      ProjectCloudExtension& projectCloudExtension, std::string_view snapshotId,
      std::string_view confirmationUrl)
   {
      auto& cloudProjectsDatabase = CloudProjectsDatabase::Get();

      auto operation = std::make_shared<ResumedSnaphotUploadOperation>(
         Tag {}, projectCloudExtension, snapshotId, confirmationUrl);

      const auto projectId = projectCloudExtension.GetCloudProjectId();

      operation->mPendingProjectBlobData =
         cloudProjectsDatabase.GetPendingProjectBlob(projectId, snapshotId);

      operation->mPendingProjectBlocks =
         cloudProjectsDatabase.GetPendingProjectBlocks(projectId, snapshotId);

      const int64_t totalBlocks = operation->mPendingProjectBlocks.size();

      projectCloudExtension.OnSyncResumed(
         operation, totalBlocks,
         operation->mPendingProjectBlobData.has_value());
   }

private:
   void UploadSnapshot()
   {
      const auto urls = UploadUrls { {},
                                     mPendingProjectBlobData->UploadUrl,
                                     mPendingProjectBlobData->ConfirmUrl,
                                     mPendingProjectBlobData->FailUrl };

      DataUploader::Get().Upload(
         mCancellationContext, GetServiceConfig(), urls,
         mPendingProjectBlobData->BlobData,
         [this, weakThis = weak_from_this()](auto result)
         {
            auto strongThis = weakThis.lock();
            if (!strongThis)
               return;

            if (result.Code != SyncResultCode::ConnectionFailed)
               CloudProjectsDatabase::Get().RemovePendingProjectBlob(
                  mProjectId, mSnapshotId);

            if (result.Code == SyncResultCode::Success)
            {
               mProjectCloudExtension.OnProjectDataUploaded(*this);
               UploadBlocks();
            }
            else
               FailSync(std::move(result));
         });
   }

   void CompleteSync()
   {
      mCompleted.store(true);
      mProjectCloudExtension.OnSyncCompleted(this, {});
   }

   void FailSync(CloudSyncError error)
   {
      mCompleted.store(true);
      mProjectCloudExtension.OnSyncCompleted(this, error);
   }

   void FailSync(ResponseResult result)
   {
      FailSync(CloudSyncError { DeduceError(result.Code), result.Content });
   }

   void UploadBlocks()
   {
      if (mPendingProjectBlocks.empty())
         MarkSnapshotSynced();

      auto project = mProjectCloudExtension.GetProject().lock();

      if (!project)
      {
         FailSync({ SyncResultCode::InternalClientError });
         return;
      }

      auto& waveTrackFactory   = WaveTrackFactory::Get(*project);
      auto& sampleBlockFactory = waveTrackFactory.GetSampleBlockFactory();

      std::vector<BlockUploadTask> blockTasks;
      blockTasks.reserve(mPendingProjectBlocks.size());

      for (const auto& pendingBlock : mPendingProjectBlocks)
      {
         BlockUploadTask task;

         task.BlockUrls = { {},
                            pendingBlock.UploadUrl,
                            pendingBlock.ConfirmUrl,
                            pendingBlock.FailUrl };

         task.Block.Format =
            static_cast<sampleFormat>(pendingBlock.BlockSampleFormat);
         task.Block.Hash  = pendingBlock.BlockHash;
         task.Block.Id    = pendingBlock.BlockId;
         task.Block.Block = sampleBlockFactory->CreateFromId(
            task.Block.Format, pendingBlock.BlockId);

         blockTasks.push_back(std::move(task));
      }

      mMissingBlocksUploader = MissingBlocksUploader::Create(
         mCancellationContext, GetServiceConfig(), std::move(blockTasks),
         [this, weakThis = weak_from_this()](
            const MissingBlocksUploadProgress& progress,
            const LockedBlock& block, ResponseResult blockResponseResult)
         {
            auto strongThis = weakThis.lock();
            if (!strongThis)
               return;

            if (
               blockResponseResult.Code !=
                  SyncResultCode::ConnectionFailed &&
               blockResponseResult.Code != SyncResultCode::Cancelled)
               CloudProjectsDatabase::Get().RemovePendingProjectBlock(
                  mProjectId, mSnapshotId, block.Id);

            mProjectCloudExtension.OnBlockUploaded(
               *this, block.Hash,
               blockResponseResult.Code == SyncResultCode::Success);

            const auto completed =
               progress.UploadedBlocks == progress.TotalBlocks ||
               progress.FailedBlocks != 0;

            const bool succeeded = completed && progress.FailedBlocks == 0;

            if (!completed)
               return;

            if (succeeded)
               MarkSnapshotSynced();
            else
               FailSync(std::move(blockResponseResult));
         });
   }

   void Start(UploadMode mode) override
   {
      if (mPendingProjectBlobData.has_value())
         UploadSnapshot();
      else
         UploadBlocks();
   }

   void MarkSnapshotSynced()
   {
      using namespace network_manager;
      Request request(mConfirmationUrl);

      SetCommonHeaders(request);

      auto response = NetworkManager::GetInstance().doPost(request, nullptr, 0);

      response->setRequestFinishedCallback(
         [this, response, weakThis = weak_from_this()](auto)
         {
            auto strongThis = weakThis.lock();
            if (!strongThis)
               return;

            CloudProjectsDatabase::Get().RemovePendingSnapshot(
               mProjectId, mSnapshotId);

            if (response->getError() != NetworkError::NoError)
            {
               FailSync(DeduceUploadError(*response));
               return;
            }

            CompleteSync();
         });

      mCancellationContext->OnCancelled(response);
   }

   void SetUploadData(const ProjectUploadData& data) override
   {
      // This method will never be called for resumed operations
   }

   bool IsCompleted() const override
   {
      return mCompleted.load();
   }

   void Cancel() override
   {
      mCancellationContext->Cancel();
   }

   ProjectCloudExtension& mProjectCloudExtension;

   std::string mProjectId;
   std::string mSnapshotId;
   std::string mConfirmationUrl;

   concurrency::CancellationContextPtr mCancellationContext;

   std::optional<PendingProjectBlobData> mPendingProjectBlobData;
   std::vector<PendingProjectBlockData> mPendingProjectBlocks;

   std::shared_ptr<MissingBlocksUploader> mMissingBlocksUploader;

   std::atomic<bool> mCompleted { false };
}; // class ResumedProjectUploadOperation

} // namespace

void ResumeProjectUpload(
   ProjectCloudExtension& projectCloudExtension,
   std::function<void()> onBeforeUploadStarts)
{
   auto& cloudProjectsDatabase = CloudProjectsDatabase::Get();

   auto pendingSnapshots = cloudProjectsDatabase.GetPendingSnapshots(
      projectCloudExtension.GetCloudProjectId());

   if (!pendingSnapshots.empty() && onBeforeUploadStarts)
      onBeforeUploadStarts();

   for (const auto& snapshot : pendingSnapshots)
      ResumedSnaphotUploadOperation::Perform(
         projectCloudExtension, snapshot.SnapshotId, snapshot.ConfirmUrl);
}

} // namespace audacity::cloud::audiocom::sync
