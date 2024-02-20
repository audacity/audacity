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
#include <mutex>
#include <string_view>

#include "AsynchronousOperation.h"
#include "CloudSyncError.h"
#include "CloudSyncUtils.h"
#include "NetworkUtils.h"

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
struct MissingBlocksUploadProgress;

class MissingBlocksUploader;

class CLOUD_AUDIOCOM_API LocalProjectSnapshot final :
    public ProjectUploadOperation,
    public std::enable_shared_from_this<LocalProjectSnapshot>
{
   struct Tag final
   {
   };

public:
   LocalProjectSnapshot(
      Tag, const ServiceConfig& config, const OAuthService& oauthService,
      ProjectCloudExtension& extension, bool forceCreateNewSnapshot);
   ~LocalProjectSnapshot();

   static std::shared_ptr<LocalProjectSnapshot> Create(
      const ServiceConfig& config, const OAuthService& oauthService,
      ProjectCloudExtension& extension, bool forceCreateNewProject = false);

   bool IsCompleted() const override;

   std::shared_ptr<AudacityProject> GetProject();

   void Start(const ProjectUploadData& projectData) override;
   void Cancel() override;

   using OnSnapshotCreatedCallback = std::function<void(const std::optional<CreateSnapshotResponse>&)>;

   void SetOnSnapshotCreated(OnSnapshotCreatedCallback callback);

private:
   void UploadFailed(CloudSyncError error);
   void DataUploadFailed(const ResponseResult& uploadResult);
   void DataUploadFailed(const MissingBlocksUploadProgress& uploadResult);

   void UpdateProjectSnapshot();

   void
   OnSnapshotCreated(const CreateSnapshotResponse& response, bool newProject);
   void MarkSnapshotSynced(int64_t blocksCount);

   void ExecuteOnSnapshotCreatedCallbacks(
      const std::optional<CreateSnapshotResponse>& response);

   ProjectCloudExtension& mProjectCloudExtension;
   std::weak_ptr<AudacityProject> mWeakProject;

   ProjectUploadData mProjectData;

   const ServiceConfig& mServiceConfig;
   const OAuthService& mOAuthService;

   struct ProjectBlocksLock;
   std::unique_ptr<ProjectBlocksLock> mProjectBlocksLock;

   std::unique_ptr<MissingBlocksUploader> mMissingBlockUploader;

   std::mutex mCreateSnapshotResponseMutex;
   std::optional<CreateSnapshotResponse> mCreateSnapshotResponse;
   std::mutex mOnSnapshotCreatedCallbacksMutex;
   std::vector<OnSnapshotCreatedCallback> mOnSnapshotCreatedCallbacks;

   std::atomic<bool> mCompleted { false };

   const bool mForceCreateNewProject;
};
} // namespace cloud::audiocom::sync
