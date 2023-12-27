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
}

class CLOUD_AUDIOCOM_API CloudSyncService final : public ProjectFileIOExtension
{
   CloudSyncService() = default;
   ~CloudSyncService() override = default;

   CloudSyncService(const CloudSyncService&) = delete;
   CloudSyncService(CloudSyncService&&) = delete;
   CloudSyncService& operator=(const CloudSyncService&) = delete;
   CloudSyncService& operator=(CloudSyncService&&) = delete;

public:
   struct CLOUD_AUDIOCOM_API UI : GlobalHook<UI, sync::CloudSyncUI&()>
   {
   };

   static CloudSyncService& Get();

   using GetProjectsCallback =
      std::function<void(sync::PaginatedProjectsResponse, std::string, bool)>;
   void GetProjects(int page, int pageSize, GetProjectsCallback callback);

   void SaveToCloud(AudacityProject& project);

   void OpenFromCloud(
      AudacityProject* targetProject, std::string projectId,
      std::string snapshotId = {});

private:
   bool DoCloudSave(AudacityProject& project, const std::string& title);
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
      const sync::SnapshotInfo& snapshotInfo);

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
