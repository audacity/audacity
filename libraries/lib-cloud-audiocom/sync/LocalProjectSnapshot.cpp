/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectSnapshot.cpp

  Dmitry Vedenko

**********************************************************************/
#include "LocalProjectSnapshot.h"

#include <algorithm>
#include <future>

#include "../OAuthService.h"
#include "../ServiceConfig.h"

#include "BasicUI.h"

#include "BlockHasher.h"
#include "CloudProjectsDatabase.h"
#include "DataUploader.h"
#include "MixdownUploader.h"
#include "ProjectCloudExtension.h"

#include "MemoryX.h"
#include "Project.h"
#include "SampleBlock.h"
#include "Sequence.h"
#include "Track.h"
#include "WaveClip.h"
#include "WaveTrack.h"

#include "TransactionScope.h"

#include "CodeConversions.h"

#include "IResponse.h"
#include "NetworkManager.h"
#include "Request.h"

#include "MissingBlocksUploader.h"

#include "StringUtils.h"

namespace cloud::audiocom::sync
{
struct LocalProjectSnapshot::ProjectBlocksLock final : private BlockHashCache
{
   ProjectCloudExtension& Extension;

   SampleBlockIDSet BlockIds;

   std::vector<LockedBlock> Blocks;
   std::vector<BlockUploadTask> MissingBlocks;

   std::unordered_map<int64_t, size_t> BlockIdToIndex;
   std::unordered_map<std::string, size_t> BlockHashToIndex;

   std::unique_ptr<BlockHasher> Hasher;

   std::future<void> UpdateCacheFuture;
   std::vector<std::pair<int64_t, std::string>> NewHashes;

   std::function<void()> OnBlocksLocked;

   explicit ProjectBlocksLock(
      ProjectCloudExtension& extension, AudacityProject& project,
      std::function<void()> onBlocksLocked)
       : Extension { extension }
       , OnBlocksLocked { std::move(onBlocksLocked) }
   {
      VisitBlocks(TrackList::Get(project));

      if (Extension.IsCloudProject())
      {
         CloudProjectsDatabase::Get().UpdateProjectBlockList(
            Extension.GetCloudProjectId(), BlockIds);
      }

      Hasher = std::make_unique<BlockHasher>();

      Hasher->ComputeHashes(*this, Blocks, [this] { CollectHashes(); });
   }

   ~ProjectBlocksLock() override
   {
   }

   void VisitBlocks(TrackList& tracks)
   {
      for (auto wt : tracks.Any<const WaveTrack>())
      {
         for (const auto pChannel : TrackList::Channels(wt))
         {
            for (const auto& clip : pChannel->GetAllClips())
            {
               for (size_t ii = 0, width = clip->GetWidth(); ii < width; ++ii)
               {
                  auto blocks = clip->GetSequenceBlockArray(ii);

                  for (const auto& block : *blocks)
                  {
                     if (block.sb)
                     {
                        const auto id = block.sb->GetBlockID();

                        if (!BlockIds.insert(id).second)
                           continue;

                        Blocks.push_back(
                           { id, wt->GetSampleFormat(), block.sb });

                        BlockIdToIndex[id] = Blocks.size() - 1;
                     }
                  }
               }
            }
         }
      }
   }

   void CollectHashes()
   {
      // TakeResult() will call UpdateHash() for each block
      // not found in the cache
      const auto result = Hasher->TakeResult();

      for (const auto& [id, hash] : result)
      {
         auto it = BlockIdToIndex.find(id);

         if (it == BlockIdToIndex.end())
         {
            assert(false);
            continue;
         }

         BlockHashToIndex[hash]  = BlockIdToIndex[id];
         Blocks[it->second].Hash = hash;
      }

      // This will potentially block, if the cache is being updated
      // already
      UpdateProjectHashesInCache();

      if (OnBlocksLocked)
         OnBlocksLocked();
   }

   void UpdateProjectHashesInCache()
   {
      if (!Extension.IsCloudProject())
         return;

      UpdateCacheFuture = std::async(
         std::launch::async,
         [this, hashes = std::move(NewHashes)]
         {
            CloudProjectsDatabase::Get().UpdateBlockHashes(
               Extension.GetCloudProjectId(), hashes);
         });
   }

   bool GetHash(int64_t blockId, std::string& hash) const override
   {
      if (!Extension.IsCloudProject())
         return false;

      auto cachedResult = CloudProjectsDatabase::Get().GetBlockHash(
         Extension.GetCloudProjectId(), blockId);

      if (!cachedResult)
         return false;

      hash = std::move(*cachedResult);

      return true;
   }

   void UpdateHash(int64_t blockId, const std::string& hash) override
   {
      NewHashes.emplace_back(blockId, hash);
   }

   void FillMissingBlocks(const std::vector<UploadUrls>& missingBlockUrls)
   {
      for (const auto& urls : missingBlockUrls)
      {
         auto it = BlockHashToIndex.find(ToUpper(urls.Id));

         if (it == BlockHashToIndex.end())
         {
            assert(false);
            continue;
         }

         const auto index = it->second;

         MissingBlocks.push_back(BlockUploadTask { urls, Blocks[index] });
      }
   }
};

LocalProjectSnapshot::LocalProjectSnapshot(
   Tag, const ServiceConfig& config, const OAuthService& oauthService,
   ProjectCloudExtension& extension, bool forceCreateNewProject)
    : mProjectCloudExtension { extension }
    , mWeakProject { extension.GetProject() }
    , mServiceConfig { config }
    , mOAuthService { oauthService }
    , mForceCreateNewProject { forceCreateNewProject }
{
}

LocalProjectSnapshot::~LocalProjectSnapshot()
{
}

std::shared_ptr<LocalProjectSnapshot> LocalProjectSnapshot::Create(
   const ServiceConfig& config, const OAuthService& oauthService,
   ProjectCloudExtension& extension, bool forceCreateNewProject)
{
   auto project = extension.GetProject().lock();

   if (!project)
      return {};

   auto snapshot = std::make_shared<LocalProjectSnapshot>(
      Tag {}, config, oauthService, extension, forceCreateNewProject);

   snapshot->mProjectBlocksLock = std::make_unique<ProjectBlocksLock>(
      extension, *project,
      [weakSnapshot = std::weak_ptr(snapshot)]
      {
         auto snapshot = weakSnapshot.lock();

         if (snapshot == nullptr)
            return;

         auto project = snapshot->GetProject();

         if (project == nullptr)
            return;

         snapshot->mProjectCloudExtension.OnBlocksHashed(*snapshot);
      });

   snapshot->mProjectCloudExtension.OnUploadOperationCreated(snapshot);

   return snapshot;
}

bool LocalProjectSnapshot::IsCompleted() const
{
   return mCompleted.load(std::memory_order_acquire);
}

std::shared_ptr<AudacityProject> LocalProjectSnapshot::GetProject()
{
   return mWeakProject.lock();
}

void LocalProjectSnapshot::Start(const ProjectUploadData& data)
{
   mProjectData = data;
   UpdateProjectSnapshot();
}

void LocalProjectSnapshot::Cancel()
{
}

void LocalProjectSnapshot::SetOnSnapshotCreated(
   std::function<void(const CreateSnapshotResponse&)> callback)
{
   {
      auto lock = std::lock_guard { mCreateSnapshotResponseMutex };

      if (mCreateSnapshotResponse)
      {
         if (callback)
            callback(*mCreateSnapshotResponse);
      }
      else if (callback)
      {
         auto lock = std::lock_guard { mOnSnapshotCreatedCallbacksMutex };
         mOnSnapshotCreatedCallbacks.push_back(std::move(callback));
      }
   }
}

void LocalProjectSnapshot::UploadFailed(CloudSyncError error)
{
   mCompleted.store(true, std::memory_order_release);
   mProjectCloudExtension.OnSyncCompleted(this, std::make_optional(error));
}

namespace
{
CloudSyncError::ErrorType DeduceError(ResponseResultCode code)
{
   switch (code)
   {
   case ResponseResultCode::Success:
      return CloudSyncError::None;
   case ResponseResultCode::Cancelled:
      return CloudSyncError::Cancelled;
   case ResponseResultCode::Expired:
      return CloudSyncError::DataUploadFailed;
   case ResponseResultCode::Conflict:
      return CloudSyncError::ProjectVersionConflict;
   case ResponseResultCode::ConnectionFailed:
      return CloudSyncError::Network;
   case ResponseResultCode::PaymentRequired:
      return CloudSyncError::ProjectStorageLimitReached;
   case ResponseResultCode::TooLarge:
      return CloudSyncError::ProjectStorageLimitReached;
   case ResponseResultCode::Unauthorized:
      return CloudSyncError::Authorization;
   case ResponseResultCode::Forbidden:
      return CloudSyncError::Authorization;
   case ResponseResultCode::NotFound:
      return CloudSyncError::ProjectNotFound;
   case ResponseResultCode::UnexpectedResponse:
      return CloudSyncError::Server;
   case ResponseResultCode::InternalClientError:
      return CloudSyncError::ClientFailure;
   case ResponseResultCode::UnknownError:
      return CloudSyncError::DataUploadFailed;
   }

   return CloudSyncError::DataUploadFailed;
}
} // namespace

void LocalProjectSnapshot::DataUploadFailed(const ResponseResult& uploadResult)
{
   UploadFailed({ DeduceError(uploadResult.Code), uploadResult.Content });
}

void LocalProjectSnapshot::DataUploadFailed(
   const MissingBlocksUploadProgress& uploadResult)
{
   CloudSyncError::ErrorType errorType = CloudSyncError::DataUploadFailed;

   for (const auto& uploadError : uploadResult.UploadErrors)
   {
      if (
         uploadError.Code == ResponseResultCode::Success ||
         uploadError.Code == ResponseResultCode::Conflict)
         continue;

      const auto deducedError = DeduceError(uploadError.Code);

      if (
         errorType == CloudSyncError::DataUploadFailed &&
         deducedError == CloudSyncError::Network)
      {
         errorType = deducedError;
      }
      else if (
         deducedError == CloudSyncError::ProjectStorageLimitReached)
      {
         errorType = deducedError;
         break;
      }
   }

   UploadFailed({ errorType, {} });
}

void LocalProjectSnapshot::UpdateProjectSnapshot()
{
   auto project = mWeakProject.lock();

   if (project == nullptr)
   {
      UploadFailed(MakeClientFailure(
         XO("Project was closed before snapshot was created")));
      return;
   }

   const bool isCloudProject = mProjectCloudExtension.IsCloudProject();
   const bool createNew      = mForceCreateNewProject || !isCloudProject;

   ProjectForm projectForm;

   if (createNew)
      projectForm.Name = audacity::ToUTF8(project->GetProjectName());
   else
      projectForm.HeadSnapshotId = mProjectCloudExtension.GetSnapshotId();

   projectForm.Hashes.reserve(mProjectBlocksLock->Blocks.size());
   std::transform(
      mProjectBlocksLock->Blocks.begin(), mProjectBlocksLock->Blocks.end(),
      std::back_inserter(projectForm.Hashes),
      [](const auto& block) { return block.Hash; });

   using namespace audacity::network_manager;

   const auto url = createNew ? mServiceConfig.GetCreateProjectUrl() :
                                mServiceConfig.GetCreateSnapshotUrl(
                                   mProjectCloudExtension.GetCloudProjectId());

   auto request = Request(url);

   request.setHeader(
      common_headers::ContentType, common_content_types::ApplicationJson);
   request.setHeader(
      common_headers::Accept, common_content_types::ApplicationJson);
   // request.setHeader(common_headers::ContentEncoding, "gzip");

   const auto language = mServiceConfig.GetAcceptLanguageValue();

   if (!language.empty())
      request.setHeader(
         audacity::network_manager::common_headers::AcceptLanguage, language);

   request.setHeader(
      common_headers::Authorization, mOAuthService.GetAccessToken());

   auto serializedForm = SerializeProjectForm(projectForm);

   auto response = NetworkManager::GetInstance().doPost(
      request, serializedForm.data(), serializedForm.size());

   response->setRequestFinishedCallback(
      [this, response, createNew](auto)
      {
         const auto error = response->getError();

         if (error != NetworkError::NoError)
         {
            UploadFailed(DeduceUploadError(*response));
            return;
         }

         const auto body = response->readAll<std::string>();
         auto result     = DeserializeCreateSnapshotResponse(body);

         if (!result)
         {
            UploadFailed(MakeClientFailure(
               XO("Invalid Response: %s").Format(body).Translation()));

            return;
         }

         OnSnapshotCreated(*result, createNew);
      });
}

void LocalProjectSnapshot::OnSnapshotCreated(
   const CreateSnapshotResponse& response, bool newProject)
{
   auto project = mWeakProject.lock();

   if (project == nullptr)
   {
      UploadFailed(MakeClientFailure(
         XO("Project was closed before snapshot was created")));
      return;
   }

   if (newProject)
      mProjectBlocksLock->UpdateProjectHashesInCache();

   mProjectBlocksLock->FillMissingBlocks(response.SyncState.MissingBlocks);

   mProjectCloudExtension.OnSnapshotCreated(*this, response);

   {
      auto lock = std::lock_guard { mCreateSnapshotResponseMutex };
      mCreateSnapshotResponse = response;
   }

   decltype(mOnSnapshotCreatedCallbacks) callbacks;

   {
      auto lock = std::lock_guard { mOnSnapshotCreatedCallbacksMutex };
      std::swap(callbacks, mOnSnapshotCreatedCallbacks);
   }

   std::for_each(
      callbacks.begin(), callbacks.end(),
      [response](auto& callback) { callback(response); });

   DataUploader::Get().Upload(
      mServiceConfig, response.SyncState.FileUrls, mProjectData.ProjectSnapshot,
      [this](ResponseResult result)
      {
         if (result.Code != ResponseResultCode::Success)
         {
            DataUploadFailed(result);
            return;
         }

         mProjectCloudExtension.OnProjectDataUploaded(*this);

         if (mProjectBlocksLock->MissingBlocks.empty())
         {
            MarkSnapshotSynced(0);
            return;
         }

         mMissingBlockUploader = std::make_unique<MissingBlocksUploader>(
            mServiceConfig, mProjectBlocksLock->MissingBlocks,
            [this](auto result, auto block, auto uploadResult)
            {
               const auto handledBlocks =
                  result.UploadedBlocks + result.FailedBlocks;

               mProjectCloudExtension.OnBlockUploaded(
                  *this, block.Hash,
                  uploadResult.Code == ResponseResultCode::Success);

               const auto completed = handledBlocks == result.TotalBlocks;
               const bool succeeded = completed && result.FailedBlocks == 0;

               if (succeeded)
               {
                  MarkSnapshotSynced(handledBlocks);
                  return;
               }

               if (completed && !succeeded)
                  DataUploadFailed(result);
            });
      });
}

void LocalProjectSnapshot::MarkSnapshotSynced(int64_t blocksCount)
{
   using namespace audacity::network_manager;
   Request request(mServiceConfig.GetSnapshotSyncUrl(
      mProjectCloudExtension.GetCloudProjectId(),
      mProjectCloudExtension.GetSnapshotId()));

   const auto language = mServiceConfig.GetAcceptLanguageValue();

   if (!language.empty())
      request.setHeader(
         audacity::network_manager::common_headers::AcceptLanguage, language);

   request.setHeader(
      common_headers::Authorization, mOAuthService.GetAccessToken());

   auto response = NetworkManager::GetInstance().doPost(request, nullptr, 0);

   response->setRequestFinishedCallback(
      [this, response, blocksCount](auto)
      {
         if (response->getError() != NetworkError::NoError)
         {
            UploadFailed(DeduceUploadError(*response));
            return;
         }

         mCompleted.store(true, std::memory_order_release);
         mProjectCloudExtension.OnSyncCompleted(this, {});
      });
}

} // namespace cloud::audiocom::sync
