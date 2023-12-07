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

struct SnapshotOperationStatus final
{
   int64_t SampleBlocksUploaded { 0 };
   int64_t SampleBlocksCount { 0 };

   bool ProjectBlobUploaded { false };
   bool Completed { false };
   bool Successful { false };

   std::string ErrorMessage;
};

class MissingBlocksUploader;

using SnapshotOperationUpdated = std::function<void(const SnapshotOperationStatus&)>;

class CloudProjectSnapshot final : public std::enable_shared_from_this<CloudProjectSnapshot>
{
   struct Tag final {};

public:
   CloudProjectSnapshot(
      Tag, const ServiceConfig& config, const OAuthService& authService,
      ProjectCloudExtension& extension, SnapshotOperationUpdated callback);
   ~CloudProjectSnapshot();

   static std::shared_ptr<CloudProjectSnapshot> Create(
      const ServiceConfig& config, const OAuthService& authService,
      ProjectCloudExtension& extension, SnapshotOperationUpdated callback,
      bool forceCreateNewProject = false);

private:
   void NotifyUpdate();

   void UpdateProjectSnapshot(bool forceCreateNew);

   void OnSnapshotCreated(const ProjectResponse& response, bool newProject);

   ProjectCloudExtension& mProjectCloudExtension;
   std::weak_ptr<AudacityProject> mWeakProject;

   const ServiceConfig& mServiceConfig;
   const OAuthService& mOAuthService;

   SnapshotOperationUpdated mUpdateCallback;

   struct ProjectBlocksLock;
   std::unique_ptr<ProjectBlocksLock> mProjectBlocksLock;

   std::unique_ptr<MissingBlocksUploader> mMissingBlockUploader;

   std::atomic<bool> mProjectUploaded { false };
};
} // namespace cloud::audiocom::sync
