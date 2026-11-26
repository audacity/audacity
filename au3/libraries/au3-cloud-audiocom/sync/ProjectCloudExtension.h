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

#include "au3-registries/ClientData.h"
#include "au3-utility/Observer.h"

#include "CloudProjectsDatabase.h"
#include "CloudSyncError.h"
#include "CloudSyncDTO.h"

class AudacityProject;

enum class AudiocomTrace;

namespace audacity::cloud::audiocom::sync {
class SnapshotUploadQueue;

enum class UploadMode
{
    Normal,
    CreateNew,
    ForceOverwrite,
};

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

    bool EnqueueSnapshot(const std::string& name, UploadMode mode);
    bool ResumeSync(const std::string& name);
    void CancelSync();

    bool IsSyncing() const;

    std::string GetCloudProjectId() const;
    std::string GetSnapshotId() const;

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
    friend class SnapshotUploadQueue;

    struct CloudStatusChangedNotifier;

    void UpdateIdFromDatabase();
    bool IsLocalAheadOfCloud() const;

    void RecordSnapshotRequested();
    void RecordSnapshotCreated(const ProjectInfo& project, const SnapshotInfo& snapshot);
    void MarkProjectSynced(DBProjectData::SyncStatusType syncStatus);

    void Publish(CloudStatusChangedMessage cloudStatus, bool canMerge);

    void OnProjectPathChanged();

    AudacityProject& mProject;

    Observer::Subscription mProjectPathChangedSubscription;

    mutable std::mutex mIdentifiersMutex;
    std::string mProjectId;
    std::string mSnapshotId;

    std::shared_ptr<SnapshotUploadQueue> mQueue;

    mutable std::mutex mStatusMutex;
    CloudStatusChangedMessage mLastStatus;

    std::unique_ptr<CloudStatusChangedNotifier> mAsyncStateNotifier;
    std::unique_ptr<CloudStatusChangedNotifier> mUIStateNotifier;
    std::atomic<bool> mUINotificationPending { false };
}; // class ProjectCloudExtension
} // namespace audacity::cloud::audiocom::sync
