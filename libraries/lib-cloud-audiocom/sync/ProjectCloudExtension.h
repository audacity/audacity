/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectExtension.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <atomic>
#include <cstdint>
#include <functional>
#include <memory>
#include <mutex>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "ClientData.h"
#include "Observer.h"

#include "AsynchronousOperation.h"
#include "CloudSyncError.h"
#include "CloudSyncUtils.h"

class AudacityProject;
class ProjectSerializer;

namespace cloud::audiocom::sync
{
class ProjectUploadOperation;

enum class ProjectSyncStatus
{
   Local,
   Unsynced,
   Synced,
   Failed,
   Syncing
};

struct CLOUD_AUDIOCOM_API CloudStatusChangedMessage final
{
   ProjectSyncStatus Status { ProjectSyncStatus::Local };
   double Progress {};
   std::optional<CloudSyncError> Error {};

   bool IsSyncing() const noexcept;
};

class CLOUD_AUDIOCOM_API ProjectCloudExtension final : public ClientData::Base
{
public:
   explicit ProjectCloudExtension(AudacityProject& project);
   ~ProjectCloudExtension() override;

   static ProjectCloudExtension& Get(AudacityProject& project);
   static const ProjectCloudExtension& Get(const AudacityProject& project);

   bool IsCloudProject() const;

   void OnLoad();

   //! This method is called from the UI thread
   void OnSyncStarted();
   //! This method is called from the UI thread
   void OnUploadOperationCreated(
      std::shared_ptr<ProjectUploadOperation> uploadOperation);
   //! This method is called not from the UI thread
   void OnBlocksHashed(ProjectUploadOperation& uploadOperation);
   //! This method is called from the network thread
   void OnSnapshotCreated(
      const ProjectUploadOperation& uploadOperation,
      const CreateSnapshotResponse& response);
   //! This method is called from the network thread
   void OnProjectDataUploaded(const ProjectUploadOperation& uploadOperation);
   //! This method is called from the network thread
   void OnBlockUploaded(
      const ProjectUploadOperation& uploadOperation, std::string_view blockID,
      bool successful);
   //! This method is called from any thread
   void OnSyncCompleted(
      const ProjectUploadOperation* uploadOperation,
      std::optional<CloudSyncError> error);

   void CancelSync();

   bool IsSyncing() const;

   std::string GetCloudProjectId() const;
   std::string GetSnapshotId() const;

   void OnUpdateSaved(const ProjectSerializer& serializer);

   std::weak_ptr<AudacityProject> GetProject() const;

   void SuppressAutoDownload();

   bool GetAutoDownloadSuppressed() const;

   void MarkNeedsMixdownSync();
   bool NeedsMixdownSync() const;
   void MixdownSynced();

   int64_t GetSavesCount() const;
   int64_t GetSavesCountSinceMixdown() const;

   Observer::Subscription SubscribeStatusChanged(
      std::function<void(const CloudStatusChangedMessage&)> callback,
      bool onUIThread);

   void MarkPendingCloudSave();
   bool IsPendingCloudSave() const;

   void SetUploadModeForNextSave(UploadMode mode);

   std::string GetCloudProjectPage() const;

private:
   struct UploadQueueElement;
   struct CloudStatusChangedNotifier;

   void UpdateIdFromDatabase();

   void UnsafeUpdateProgress();
   void Publish(CloudStatusChangedMessage cloudStatus, bool canMerge);

   void MarkProjectSynced(bool success);

   UploadQueueElement*
   UnsafeFindUploadQueueElement(const ProjectUploadOperation& uploadOperation);
   const UploadQueueElement* UnsafeFindUploadQueueElement(
      const ProjectUploadOperation& uploadOperation) const;

   AudacityProject& mProject;

   mutable std::mutex mIdentifiersMutex;
   std::string mProjectId;
   std::string mSnapshotId;

   std::mutex mUploadQueueMutex;
   std::vector<std::shared_ptr<UploadQueueElement>> mUploadQueue;

   std::mutex mStatusMutex;
   CloudStatusChangedMessage mLastStatus;

   std::unique_ptr<CloudStatusChangedNotifier> mAsyncStateNotifier;
   std::unique_ptr<CloudStatusChangedNotifier> mUIStateNotifier;
   std::atomic<bool> mUINotificationPending { false };

   UploadMode mNextUploadMode { UploadMode::Normal };

   bool mSuppressAutoDownload { false };
   bool mPendingCloudSave { false };
   bool mNeedsMixdownSync { false };
};
} // namespace cloud::audiocom::sync
