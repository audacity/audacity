/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSyncService.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "ProjectFileIOExtension.h"

#include <functional>
#include <string>
#include <string_view>
#include <mutex>
#include <memory>
#include <vector>

#include "GlobalVariable.h"


namespace cloud::audiocom
{
namespace sync
{
class LocalProjectSnapshot;
class CloudSyncUI;
class PaginatedProjectsResponse;
class ProjectInfo;
class SnapshotInfo;
class RemoteProjectSnapshot;

enum class ProjectDownloadState
{
   Failed,
   Cancelled,
   NotAuthorized,
   Succeeded,
};

struct ProjectDownloadResult final
{
   AudacityProject* TargetProject {};
   std::string ProjectPath;
   std::string ErrorMessage;
   ProjectDownloadState SyncState { ProjectDownloadState::Failed };
}; // struct ProjectDownloadResult

using ProjectDownloadedCallback =
   std::function<void(ProjectDownloadResult result)>;

using GetProjectsCallback =
   std::function<void(sync::PaginatedProjectsResponse, std::string, bool)>;
}

//! CloudSyncService is responsible for saving and loading projects from the cloud
class CLOUD_AUDIOCOM_API CloudSyncService final : public ProjectFileIOExtension
{
   CloudSyncService() = default;
   ~CloudSyncService() override = default;

   CloudSyncService(const CloudSyncService&) = delete;
   CloudSyncService(CloudSyncService&&) = delete;
   CloudSyncService& operator=(const CloudSyncService&) = delete;
   CloudSyncService& operator=(CloudSyncService&&) = delete;

public:
   struct CLOUD_AUDIOCOM_API UI
      : GlobalHook<UI, sync::CloudSyncUI&()> {};

   static CloudSyncService& Get();

   //! Retrieve the list of projects from the cloud
   void GetProjects(int page, int pageSize, sync::GetProjectsCallback callback);

   //! Save the project to the cloud. This operation is asynchronous.
   void SaveToCloud(AudacityProject& project);

   //! Open the project from the cloud. This operation is asynchronous.
   void OpenFromCloud(
      AudacityProject* targetProject, std::string projectId,
      std::string snapshotId, sync::ProjectDownloadedCallback callback);

private:
   bool DoCloudSave(AudacityProject& project, const std::string& title);

   void OnOpen(AudacityProject& project, const std::string& path) override;
   void OnLoad(AudacityProject& project) override;
   bool OnSave(AudacityProject& project, bool fromTempProject) override;
   bool OnClose(AudacityProject& project) override;
   bool IsBlockLocked(
      const AudacityProject& project, int64_t blockId) const override;
   void OnUpdateSaved(
      AudacityProject& project, const ProjectSerializer& serializer) override;

   void CreateSnapshot(AudacityProject& project);

   void SyncCloudSnapshot(
      AudacityProject* targetProject,
      const sync::ProjectInfo& projectInfo,
      const sync::SnapshotInfo& snapshotInfo,
      sync::ProjectDownloadedCallback callback);

   sync::CloudSyncUI& GetUI() const;

   void
   UpdateProgress(double downloadProgress = -1.0);

   std::vector<std::shared_ptr<sync::LocalProjectSnapshot>> mLocalSnapshots;
   std::shared_ptr<sync::RemoteProjectSnapshot> mRemoteSnapshot;

   std::mutex mProgressMutex;
   double mDownloadProgress { 0.0 };
   bool mProgressUpdateQueued { false };
};
} // namespace cloud::audiocom
