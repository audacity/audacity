/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectSnapshot.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <atomic>
#include <functional>
#include <memory>
#include <string_view>

#include "CloudSyncUtils.h"

class AudacityProject;

namespace cloud::audiocom
{
class OAuthService;
class ServiceConfig;
} // namespace cloud::audiocom

namespace cloud::audiocom::sync
{
constexpr auto UNASSIGNED_PROJECT_ID = -1;

class ProjectCloudExtension;
class LocalProjectSnapshot;
class MixdownUploader;
class CloudSyncUI;

struct SnapshotOperationStatus final
{
   LocalProjectSnapshot* Snapshot { nullptr };

   int64_t SampleBlocksProcessed { 0 };
   int64_t SampleBlocksCount { 0 };

   bool ProjectBlobProcessed { false };
   bool Completed { false };
   bool Successful { false };

   std::string ErrorMessage;
};

class MissingBlocksUploader;

using SnapshotOperationUpdated = std::function<void(const SnapshotOperationStatus&)>;

class LocalProjectSnapshot final : public std::enable_shared_from_this<LocalProjectSnapshot>
{
   struct Tag final {};

public:
   LocalProjectSnapshot(
      Tag, CloudSyncUI& ui, const ServiceConfig& config,
      const OAuthService& authService, ProjectCloudExtension& extension,
      SnapshotOperationUpdated callback);
   ~LocalProjectSnapshot();

   static std::shared_ptr<LocalProjectSnapshot> Create(
      CloudSyncUI& ui, const ServiceConfig& config,
      const OAuthService& authService, ProjectCloudExtension& extension,
      SnapshotOperationUpdated callback, bool forceCreateNewProject = false);

   bool IsCompleted() const;

   double GetSyncProgress() const;

   std::shared_ptr<AudacityProject> GetProject();

   void Cancel();

private:
   void SetSyncProgress(int64_t uploadedBlocks, int64_t totalBlocks);

   void UpdateProjectSnapshot(bool forceCreateNew);

   void OnSnapshotCreated(const CreateProjectResponse& response, bool newProject);
   void MarkSnapshotSynced(int64_t blocksCount);

   ProjectCloudExtension& mProjectCloudExtension;
   std::weak_ptr<AudacityProject> mWeakProject;

   CloudSyncUI& mCloudSyncUI;
   const ServiceConfig& mServiceConfig;
   const OAuthService& mOAuthService;

   SnapshotOperationUpdated mUpdateCallback;

   struct ProjectBlocksLock;
   std::unique_ptr<ProjectBlocksLock> mProjectBlocksLock;

   std::unique_ptr<MissingBlocksUploader> mMissingBlockUploader;

   std::unique_ptr<MixdownUploader> mMixdownUploader;

   std::atomic<int64_t> mUploadedBlocks { 0 };
   std::atomic<int64_t> mTotalBlocks { 0 };

   std::atomic<bool> mProjectUploaded { false };

   std::atomic<bool> mCompleted { false };
   std::atomic<bool> mMixdownUploadInProgress { false };
};
} // namespace cloud::audiocom::sync
