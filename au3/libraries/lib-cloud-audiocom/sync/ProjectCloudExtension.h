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
#include <vector>

#include "ClientData.h"
#include "Observer.h"

#include "CloudSyncError.h"
#include "CloudSyncDTO.h"
#include "ProjectUploadOperation.h"

class AudacityProject;
class ProjectSerializer;

enum class AudiocomTrace;

namespace audacity::cloud::audiocom::sync {
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
    AudiocomTrace audiocomTrace;
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
    void OnSyncResumed(
        std::shared_ptr<ProjectUploadOperation> uploadOperation, int64_t missingBlocksCount, bool needsProjectUpload);
    //! This method is called from the UI thread
    void OnUploadOperationCreated(
        std::shared_ptr<ProjectUploadOperation> uploadOperation);
    //! This method is called not from the UI thread
    void OnBlocksHashed(ProjectUploadOperation& uploadOperation);
    //! This method is called from the network thread
    void OnSnapshotCreated(
        const ProjectUploadOperation& uploadOperation, const CreateSnapshotResponse& response);
    //! This method is called from the network thread
    void OnProjectDataUploaded(const ProjectUploadOperation& uploadOperation);
    //! This method is called from the network thread
    void OnBlockUploaded(
        const ProjectUploadOperation& uploadOperation, std::string_view blockID, bool successful);
    //! This method is called from any thread
    void OnSyncCompleted(
        const ProjectUploadOperation* uploadOperation, std::optional<CloudSyncError> error, AudiocomTrace trace);

    void AbortLastUploadOperation();

    void CancelSync();

    bool IsSyncing() const;

    std::string GetCloudProjectId() const;
    std::string GetSnapshotId() const;

    void OnUpdateSaved(const ProjectSerializer& serializer);

    std::weak_ptr<AudacityProject> GetProject() const;

    int64_t GetSavesCount() const;

    Observer::Subscription SubscribeStatusChanged(
        std::function<void(const CloudStatusChangedMessage&)> callback, bool onUIThread);

    std::string GetCloudProjectPage(AudiocomTrace) const;

    bool IsBlockLocked(int64_t blockID) const;

    ProjectSyncStatus GetCurrentSyncStatus() const;

    bool IsFirstSyncDialogShown() const;
    void SetFirstSyncDialogShown(bool shown = true);

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

    void OnProjectPathChanged();

    AudacityProject& mProject;

    Observer::Subscription mProjectPathChangedSubscription;

    mutable std::mutex mIdentifiersMutex;
    std::string mProjectId;
    std::string mSnapshotId;

    std::mutex mUploadQueueMutex;
    std::vector<std::shared_ptr<UploadQueueElement> > mUploadQueue;

    mutable std::mutex mStatusMutex;
    CloudStatusChangedMessage mLastStatus;

    std::unique_ptr<CloudStatusChangedNotifier> mAsyncStateNotifier;
    std::unique_ptr<CloudStatusChangedNotifier> mUIStateNotifier;
    std::atomic<bool> mUINotificationPending { false };
}; // class ProjectCloudExtension
} // namespace audacity::cloud::audiocom::sync
