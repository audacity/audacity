/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectSnapshot.cpp

  Dmitry Vedenko

**********************************************************************/
#include "LocalProjectSnapshot.h"

#include <algorithm>
#include <future>

#include "../ServiceConfig.h"
#include "../OAuthService.h"

#include "BasicUI.h"

#include "BlockHasher.h"
#include "CloudProjectsDatabase.h"
#include "ProjectCloudExtension.h"
#include "DataUploader.h"
#include "MixdownUploader.h"

#include "SampleBlock.h"
#include "Sequence.h"
#include "Track.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "Project.h"
#include "MemoryX.h"

#include "TransactionScope.h"

#include "CodeConversions.h"

#include "Request.h"
#include "IResponse.h"
#include "NetworkManager.h"
#include "MultipartData.h"

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

         if (it == BlockIdToIndex.end ())
         {
            assert(false);
            continue;
         }

         BlockHashToIndex[hash] = BlockIdToIndex[id];
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
         std::launch::async, [this, hashes = std::move(NewHashes)] {
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

   void FillMissingBlocks (const std::vector<UploadUrls>& missingBlockUrls)
   {
      for (const auto& urls : missingBlockUrls)
      {
         auto it = BlockHashToIndex.find(ToUpper(urls.Id));

         if (it == BlockHashToIndex.end ())
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
   Tag, CloudSyncUI& ui, const ServiceConfig& config,
   const OAuthService& authService,
   ProjectCloudExtension& extension, SnapshotOperationUpdated callback)
    : mProjectCloudExtension { extension }
    , mWeakProject { extension.GetProject() }
    , mCloudSyncUI { ui }
    , mServiceConfig { config }
    , mOAuthService { authService }
    , mUpdateCallback { std::move(callback) }
{

}

LocalProjectSnapshot::~LocalProjectSnapshot()
{
}

std::shared_ptr<LocalProjectSnapshot> LocalProjectSnapshot::Create(
   CloudSyncUI& ui, const ServiceConfig& config,
   const OAuthService& authService, ProjectCloudExtension& extension,
   SnapshotOperationUpdated callback, bool forceCreateNewProject)
{
   auto project = extension.GetProject().lock();

   if (!callback)
      callback = [](const auto) {};

   if (!project)
   {
      callback({ nullptr, 0, 0, false, false, false, { "Invalid project state" } });
      return {};
   }

   auto snapshot = std::make_shared<LocalProjectSnapshot>(
      Tag {}, ui, config, authService, extension, std::move(callback));

   snapshot->mProjectBlocksLock = std::make_unique<ProjectBlocksLock>(
      extension, *project,
      [weakSnapshot = std::weak_ptr(snapshot), forceCreateNewProject]
      {
         auto snapshot = weakSnapshot.lock();

         if (snapshot)
            snapshot->UpdateProjectSnapshot(forceCreateNewProject);
      });

   return snapshot;
}

bool LocalProjectSnapshot::IsCompleted() const
{
   return mCompleted.load(std::memory_order_acquire);
}

void LocalProjectSnapshot::SetSyncProgress(
   int64_t uploadedBlocks, int64_t totalBlocks)
{
   std::atomic_thread_fence(std::memory_order_release);
   mUploadedBlocks.store(uploadedBlocks, std::memory_order_relaxed);
   mTotalBlocks.store(totalBlocks, std::memory_order_relaxed);
}

double LocalProjectSnapshot::GetSyncProgress() const
{
   const auto uploadedBlocks = mUploadedBlocks.load(std::memory_order_relaxed);
   const auto totalBlocks = mTotalBlocks.load(std::memory_order_relaxed);
   const auto projectUploaded =
      mProjectUploaded.load(std::memory_order_relaxed) ? 1 : 0;
   std::atomic_thread_fence(std::memory_order_acquire);

   return (uploadedBlocks + projectUploaded) /
          static_cast<double>(totalBlocks + 1);
}

std::shared_ptr<AudacityProject> LocalProjectSnapshot::GetProject()
{
   return mWeakProject.lock();
}

void LocalProjectSnapshot::Cancel()
{
}

void LocalProjectSnapshot::UpdateProjectSnapshot(bool forceCreateNewProject)
{
   auto project = mWeakProject.lock();

   if (project == nullptr)
   {
      mUpdateCallback(
         { this, 0, 0, false, true, false, { "Invalid project state" } });
      return;
   }

   const bool isCloudProject = mProjectCloudExtension.IsCloudProject();
   const bool createNew = forceCreateNewProject || !isCloudProject;

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

   const auto language =
   mServiceConfig.GetAcceptLanguageValue();

   if (!language.empty())
      request.setHeader(
         audacity::network_manager::common_headers::AcceptLanguage, language);

    request.setHeader(
      common_headers::Authorization, mOAuthService.GetAccessToken());

   auto serializedForm = SerializeProjectForm(projectForm);

   auto response =
      NetworkManager::GetInstance().doPost(request, serializedForm.data(), serializedForm.size());

   response->setRequestFinishedCallback(
      [this, response, createNew](auto)
      {
         const auto error = response->getError();

         if (error != NetworkError::NoError)
         {
            auto errorMessage = response->getErrorString();

            if (error == NetworkError::HTTPError)
            {
               errorMessage +=
                  "\nHTTP code: " + std::to_string(response->getHTTPCode());
               errorMessage +=
                  "\nResponse: " + response->readAll<std::string>();
            }

            mUpdateCallback({ this, 0, 0, false, true, false, errorMessage });
         }
         else
         {
            const auto body = response->readAll<std::string>();
            auto result =
               DeserializeCreateProjectResponse(body);

            if (!result)
            {
               mUpdateCallback({ this, 0, 0, false, true, false,
                                 audacity::ToUTF8(XO("Invalid Response: %s")
                                                     .Format(body)
                                                     .Translation()) });
               return;
            }

            OnSnapshotCreated(*result, createNew);
         }
      });
}

void LocalProjectSnapshot::OnSnapshotCreated(
   const CreateProjectResponse& response, bool newProject)
{
   auto project = mWeakProject.lock();

   if (project == nullptr)
   {
      mUpdateCallback(
         { this, 0, 0, false, true, false, { "Invalid project state" } });
      return;
   }

   mProjectCloudExtension.OnSnapshotCreated(
      response.Project.Id, response.Snapshot.Id);

   if (newProject)
      mProjectBlocksLock->UpdateProjectHashesInCache();

   mProjectBlocksLock->FillMissingBlocks(response.SyncState.MissingBlocks);

   DataUploader::Get().Upload(
      mServiceConfig, response.SyncState.FileUrls,
      mProjectCloudExtension.GetUpdatedProjectContents(),
      [this](UploadResult result)
      {
         if (result.Code != UploadResultCode::Success)
         {
            mUpdateCallback({ this, 0, 0, false, true, false,
                              std::move(result.ErrorMessage) });
         }
         else
         {
            mProjectUploaded.store(true, std::memory_order_release);
            mUpdateCallback({ this, 0, 0, true, false, false,
                              std::move(result.ErrorMessage) });

            if (mProjectBlocksLock->MissingBlocks.empty())
            {
               MarkSnapshotSynced(0);
               return;
            }

            mMissingBlockUploader = std::make_unique<MissingBlocksUploader>(
               mServiceConfig, mProjectBlocksLock->MissingBlocks,
               [this](auto result, auto block, auto action)
               {
                  const auto handledBlocks =
                     result.UploadedBlocks + result.FailedBlocks;

                  const auto completed = handledBlocks == result.TotalBlocks;
                  const bool succeeded = completed && result.FailedBlocks == 0;

                  if (succeeded)
                  {
                     MarkSnapshotSynced(handledBlocks);
                     return;
                  }

                  SetSyncProgress(handledBlocks, result.TotalBlocks);

                  mUpdateCallback({ this, handledBlocks, result.TotalBlocks,
                                    true, completed, succeeded,
                                    Join(result.ErrorMessages, "\n") });

                  if (completed)
                  {
                     mCompleted.store(true, std::memory_order_release);
                     mProjectCloudExtension.OnSyncCompleted(false);
                  }
               });
         }
      });

   mMixdownUploadInProgress.store(true, std::memory_order_release);

   BasicUI::CallAfter(
      [this, mixdownUrls = response.SyncState.MixdownUrls]
      {
         auto project = mWeakProject.lock();

         if (!project)
            return;

         if (!mProjectCloudExtension.NeedsMixdownSync())
         {
            mMixdownUploadInProgress.store(false, std::memory_order_release);
            return;
         }

         mMixdownUploader = MixdownUploader::Upload(
            mCloudSyncUI, mServiceConfig, *project, mixdownUrls,
            [this](std::string, bool success)
            {
               if (success)
                  mProjectCloudExtension.MixdownSynced();

               mMixdownUploadInProgress.store(false, std::memory_order_release);
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
         const auto error = response->getError();

         const auto success = error == NetworkError::NoError;

         std::string errorMessage;

         if (!success)
         {
            auto errorMessage = response->getErrorString();

            if (error == NetworkError::HTTPError)
            {
               errorMessage +=
                  "\nHTTP code: " + std::to_string(response->getHTTPCode());
               errorMessage +=
                  "\nResponse: " + response->readAll<std::string>();
            }
         }

         mCompleted.store(true, std::memory_order_release);
         mProjectCloudExtension.OnSyncCompleted(success);

         // Wait for mixdown upload to complete
         while (mMixdownUploadInProgress.load(std::memory_order_acquire))
            std::this_thread::sleep_for(std::chrono::milliseconds(100));

         mUpdateCallback({ this, blocksCount, blocksCount, true, true, success,
                           std::move(errorMessage) });
      });
}

} // namespace cloud::audiocom::sync
