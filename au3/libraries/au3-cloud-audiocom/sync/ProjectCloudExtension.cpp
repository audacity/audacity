/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectCloudExtension.h

  Dmitry Vedenko

**********************************************************************/

#include "ProjectCloudExtension.h"

#include <algorithm>
#include <cstring>
#include <vector>

#include "CloudLibrarySettings.h"
#include "CloudSyncDTO.h"
#include "ProjectUploadOperation.h"
#include "ServiceConfig.h"

#include "CodeConversions.h"

#include "Project.h"

#include "ProjectFileIO.h"
#include "ProjectSerializer.h"

#include "BasicUI.h"
#include "ExportUtils.h"

#include "MemoryX.h"

#include "CloudProjectsDatabase.h"
#include "OAuthService.h"
#include "UserService.h"

#include "Track.h"

namespace audacity::cloud::audiocom::sync {
namespace {
const AttachedProjectObjects::RegisteredFactory key {
    [](AudacityProject& project)
    { return std::make_shared<ProjectCloudExtension>(project); }
};
} // namespace

struct ProjectCloudExtension::UploadQueueElement final
{
    ProjectUploadData Data;
    std::shared_ptr<ProjectUploadOperation> Operation;
    std::optional<CreateSnapshotResponse> SnapshotResponse;

    int64_t BlocksHandled { 0 };
    int64_t BlocksTotal { 0 };

    bool ProjectDataUploaded { false };
    bool ReadyForUpload { false };
    bool Synced { false };
};

struct ProjectCloudExtension::CloudStatusChangedNotifier final : private Observer::Publisher<CloudStatusChangedMessage>
{
    void Enqueue(CloudStatusChangedMessage message, bool canMerge)
    {
        auto lock = std::lock_guard { QueueMutex };

        if (Queue.empty() || !canMerge) {
            Queue.push_back({ message, canMerge });
        } else if (Queue.back().second) {
            Queue.back().first = message;
        } else {
            Queue.push_back({ message, canMerge });
        }
    }

    void PublishSafe()
    {
        QueueType queue;
        {
            auto lock = std::lock_guard { QueueMutex };
            std::swap(queue, Queue);
        }

        auto lock = std::lock_guard { ObserverMutex };

        for (const auto& [message, _] : queue) {
            Publish(message);
        }
    }

    Observer::Subscription
    SubscribeSafe(std::function<void(const CloudStatusChangedMessage&)> callback)
    {
        auto lock = std::lock_guard { ObserverMutex };
        return Subscribe(std::move(callback));
    }

    using QueueType = std::vector<std::pair<CloudStatusChangedMessage, bool> >;

    std::mutex QueueMutex;
    QueueType Queue;
    std::recursive_mutex ObserverMutex;
};

ProjectCloudExtension::ProjectCloudExtension(AudacityProject& project)
    : mProject{project}
    , mProjectPathChangedSubscription{ProjectFileIO::Get(project).Subscribe(
                                          [this](auto message)
    {
        if (message == ProjectFileIOMessage::ProjectFilePathChange)
            OnProjectPathChanged();
    })},
    mAsyncStateNotifier { std::make_unique<CloudStatusChangedNotifier>() },
mUIStateNotifier { std::make_unique<CloudStatusChangedNotifier>() }
{
    if (!ProjectFileIO::Get(project).IsTemporary()) {
        UpdateIdFromDatabase();
    }
}

ProjectCloudExtension::~ProjectCloudExtension() = default;

ProjectCloudExtension& ProjectCloudExtension::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get<ProjectCloudExtension&>(key);
}

const ProjectCloudExtension&
ProjectCloudExtension::Get(const AudacityProject& project)
{
    return Get(const_cast<AudacityProject&>(project));
}

bool ProjectCloudExtension::IsCloudProject() const
{
    auto lock = std::lock_guard { mIdentifiersMutex };
    return !mProjectId.empty();
}

void ProjectCloudExtension::OnLoad()
{
    UpdateIdFromDatabase();
}

void ProjectCloudExtension::OnSyncStarted()
{
    auto element = std::make_shared<UploadQueueElement>();

    element->Data.Tracks = TrackList::Create(nullptr);

    for (auto pTrack : TrackList::Get(mProject)) {
        if (pTrack->GetId() == TrackId {}) {
            // Don't copy a pending added track
            continue;
        }
        element->Data.Tracks->Add(pTrack->Duplicate());
    }

    auto lock = std::lock_guard { mUploadQueueMutex };
    mUploadQueue.push_back(std::move(element));
}

void cloud::audiocom::sync::ProjectCloudExtension::OnSyncResumed(
    std::shared_ptr<ProjectUploadOperation> uploadOperation,
    int64_t missingBlocksCount, bool needsProjectUpload)
{
    auto element                 = std::make_shared<UploadQueueElement>();
    element->Operation           = uploadOperation;
    element->BlocksTotal         = missingBlocksCount;
    element->ProjectDataUploaded = !needsProjectUpload;

    {
        auto lock = std::lock_guard { mUploadQueueMutex };
        mUploadQueue.push_back(element);
    }

    uploadOperation->Start();
    UnsafeUpdateProgress();
}

void ProjectCloudExtension::OnUploadOperationCreated(
    std::shared_ptr<ProjectUploadOperation> uploadOperation)
{
    auto lock = std::lock_guard { mUploadQueueMutex };
    assert(mUploadQueue.size() > 0);

    if (mUploadQueue.empty()) {
        return;
    }

    mUploadQueue.back()->Operation = uploadOperation;
    UnsafeUpdateProgress();
}

void ProjectCloudExtension::OnBlocksHashed(
    ProjectUploadOperation& uploadOperation)
{
    auto lock    = std::lock_guard { mUploadQueueMutex };
    auto element = UnsafeFindUploadQueueElement(uploadOperation);

    if (!element) {
        return;
    }

    element->ReadyForUpload = true;
    uploadOperation.Start();
}

void ProjectCloudExtension::OnSnapshotCreated(
    const ProjectUploadOperation& uploadOperation,
    const CreateSnapshotResponse& response)
{
    auto& cloudDatabase = CloudProjectsDatabase::Get();

    const auto projectFilePath
        =audacity::ToUTF8(ProjectFileIO::Get(mProject).GetFileName());

    auto previousDbData = cloudDatabase.GetProjectDataForPath(projectFilePath);

    DBProjectData dbData;

    if (previousDbData) {
        dbData = *previousDbData;
    }

    dbData.ProjectId    = response.Project.Id;
    dbData.SnapshotId   = response.Snapshot.Id;
    dbData.LocalPath    = projectFilePath;
    dbData.LastModified = wxDateTime::Now().GetTicks();
    dbData.LastRead     = dbData.LastModified;
    dbData.SyncStatus   = DBProjectData::SyncStatusUploading;
    dbData.SavesCount++;

    cloudDatabase.UpdateProjectData(dbData);
    cloudDatabase.SetProjectUserSlug(
        response.Project.Id, response.Project.Username);

    {
        auto lock = std::lock_guard { mIdentifiersMutex };

        mProjectId  = response.Project.Id;
        mSnapshotId = response.Snapshot.Id;
    }

    auto lock    = std::lock_guard { mUploadQueueMutex };
    auto element = UnsafeFindUploadQueueElement(uploadOperation);

    if (!element) {
        return;
    }

    element->BlocksTotal      = response.SyncState.MissingBlocks.size();
    element->SnapshotResponse = response;

    UnsafeUpdateProgress();
}

void ProjectCloudExtension::OnProjectDataUploaded(
    const ProjectUploadOperation& uploadOperation)
{
    auto lock    = std::lock_guard { mUploadQueueMutex };
    auto element = UnsafeFindUploadQueueElement(uploadOperation);

    if (!element) {
        return;
    }

    element->ProjectDataUploaded = true;

    UnsafeUpdateProgress();
}

void ProjectCloudExtension::OnBlockUploaded(
    const ProjectUploadOperation& uploadOperation, std::string_view blockID,
    bool successful)
{
    auto lock    = std::lock_guard { mUploadQueueMutex };
    auto element = UnsafeFindUploadQueueElement(uploadOperation);

    if (!element) {
        return;
    }

    element->BlocksHandled++;

    UnsafeUpdateProgress();
}

void ProjectCloudExtension::OnSyncCompleted(
    const ProjectUploadOperation* uploadOperation,
    std::optional<CloudSyncError> error, AudiocomTrace trace)
{
    auto lock = std::lock_guard { mUploadQueueMutex };

    auto element = uploadOperation != nullptr
                   ? UnsafeFindUploadQueueElement(*uploadOperation)
                   : nullptr;

    if (element != nullptr) {
        element->Synced = true;

        if (
            error.has_value() && error->Type == CloudSyncError::Aborted
            && element->SnapshotResponse) {
            auto& cloudDatabase = CloudProjectsDatabase::Get();

            const auto projectFilePath
                =audacity::ToUTF8(ProjectFileIO::Get(mProject).GetFileName());

            auto previousDbData
                =cloudDatabase.GetProjectDataForPath(projectFilePath);

            DBProjectData dbData;

            if (previousDbData) {
                dbData = *previousDbData;
            }

            const auto parentId = element->SnapshotResponse->Snapshot.ParentId;

            dbData.SnapshotId = parentId;

            cloudDatabase.UpdateProjectData(dbData);

            {
                auto lock   = std::lock_guard { mIdentifiersMutex };
                mSnapshotId = parentId;
            }
        }
    }

    // std::all_of returns true if the range is empty
    if (!std::all_of(
            mUploadQueue.begin(), mUploadQueue.end(),
            [](auto& operation) { return operation->Synced; })) {
        UnsafeUpdateProgress();
        return;
    }

    mUploadQueue.clear();

    MarkProjectSynced(!error.has_value());

    Publish(
        { trace,
          error.has_value() ? ProjectSyncStatus::Failed
          : ProjectSyncStatus::Synced,
          {},
          error },
        false);

    if (!IsCloudProject()) {
        Publish(
            { AudiocomTrace::ignore, // All right
              ProjectSyncStatus::Local },
            false);
    }
}

void ProjectCloudExtension::CancelSync()
{
    std::vector<std::shared_ptr<UploadQueueElement> > queue;

    {
        auto lock = std::lock_guard { mUploadQueueMutex };
        std::swap(queue, mUploadQueue);
    }

    if (queue.empty()) {
        return;
    }

    for (auto& item : queue) {
        if (!item->Operation) {
            continue;
        }

        item->Operation->Cancel();
    }
}

bool ProjectCloudExtension::IsSyncing() const
{
    auto lock = std::lock_guard { mStatusMutex };

    return mLastStatus.Status == ProjectSyncStatus::Syncing;
}

std::string ProjectCloudExtension::GetCloudProjectId() const
{
    auto lock = std::lock_guard { mIdentifiersMutex };

    return mProjectId;
}

std::string ProjectCloudExtension::GetSnapshotId() const
{
    auto lock = std::lock_guard { mIdentifiersMutex };

    return mSnapshotId;
}

void ProjectCloudExtension::OnUpdateSaved(const ProjectSerializer& serializer)
{
    auto lock = std::lock_guard { mUploadQueueMutex };

    if (mUploadQueue.empty()) {
        return;
    }

    const size_t dictSize    = serializer.GetDict().GetSize();
    const size_t projectSize = serializer.GetData().GetSize();

    std::vector<uint8_t> data;
    data.resize(projectSize + dictSize + sizeof(uint64_t));

    const uint64_t dictSizeData
        =IsLittleEndian() ? dictSize : SwapIntBytes(dictSize);

    std::memcpy(data.data(), &dictSizeData, sizeof(uint64_t));

    uint64_t offset = sizeof(uint64_t);

    for (const auto [chunkData, size] : serializer.GetDict()) {
        std::memcpy(data.data() + offset, chunkData, size);
        offset += size;
    }

    for (const auto [chunkData, size] : serializer.GetData()) {
        std::memcpy(data.data() + offset, chunkData, size);
        offset += size;
    }

    mUploadQueue.back()->Data.ProjectSnapshot = std::move(data);

    if (mUploadQueue.back()->Operation) {
        mUploadQueue.back()->Operation->SetUploadData(mUploadQueue.back()->Data);

        auto dbData = CloudProjectsDatabase::Get().GetProjectData(mProjectId);

        if (dbData) {
            dbData->LocalPath
                =audacity::ToUTF8(ProjectFileIO::Get(mProject).GetFileName());
            CloudProjectsDatabase::Get().UpdateProjectData(*dbData);
        }
    } else {
        Publish(
            { AudiocomTrace::ignore, // TODO Is this correct ?
              ProjectSyncStatus::Failed },
            false);
    }
}

std::weak_ptr<AudacityProject> ProjectCloudExtension::GetProject() const
{
    return mProject.weak_from_this();
}

int64_t ProjectCloudExtension::GetSavesCount() const
{
    auto lock = std::lock_guard { mIdentifiersMutex };

    if (mProjectId.empty()) {
        return 0;
    }

    auto& cloudDatabase = CloudProjectsDatabase::Get();
    auto dbData         = cloudDatabase.GetProjectData(mProjectId);

    if (!dbData) {
        return 0;
    }

    return dbData->SavesCount;
}

Observer::Subscription ProjectCloudExtension::SubscribeStatusChanged(
    std::function<void(const CloudStatusChangedMessage&)> callback,
    bool onUIThread)
{
    return onUIThread ? mUIStateNotifier->SubscribeSafe(std::move(callback))
           : mAsyncStateNotifier->SubscribeSafe(std::move(callback));
}

void ProjectCloudExtension::UpdateIdFromDatabase()
{
    auto& projectFileIO = ProjectFileIO::Get(mProject);
    auto& cloudDatabase = CloudProjectsDatabase::Get();

    auto projectData = cloudDatabase.GetProjectDataForPath(
        audacity::ToUTF8(projectFileIO.GetFileName()));

    if (!projectData) {
        return;
    }

    {
        auto lock = std::lock_guard { mIdentifiersMutex };

        mProjectId  = projectData->ProjectId;
        mSnapshotId = projectData->SnapshotId;
    }

    Publish(
        {
            AudiocomTrace::ignore, // OK here because we're not publishing an
                                   // error
            projectData->SyncStatus
            == DBProjectData::SyncStatusType::SyncStatusSynced
            ? ProjectSyncStatus::Synced
            : ProjectSyncStatus::Unsynced,
        },
        false);
}

void ProjectCloudExtension::UnsafeUpdateProgress()
{
    if (mUploadQueue.empty()) {
        return;
    }

    int64_t handledElements = 0;
    int64_t totalElements   = 0;

    for (auto& element : mUploadQueue) {
        handledElements
            +=element->BlocksHandled + int(element->ProjectDataUploaded);
        totalElements += element->BlocksTotal + 1;
    }

    assert(totalElements > 0);

    Publish(
        { AudiocomTrace::ignore, // All right
          ProjectSyncStatus::Syncing,
          double(handledElements) / double(totalElements) },
        true);
}

void ProjectCloudExtension::Publish(
    CloudStatusChangedMessage cloudStatus, bool canMerge)
{
    {
        auto lock   = std::lock_guard { mStatusMutex };
        mLastStatus = cloudStatus;
    }

    mAsyncStateNotifier->Enqueue(cloudStatus, canMerge);
    mUIStateNotifier->Enqueue(cloudStatus, canMerge);

    mAsyncStateNotifier->PublishSafe();

    if (BasicUI::IsUiThread()) {
        mUINotificationPending.store(false);
        mUIStateNotifier->PublishSafe();
    } else if (!mUINotificationPending.exchange(true)) {
        BasicUI::CallAfter(
            [this]
        {
            if (mUINotificationPending.exchange(false)) {
                mUIStateNotifier->PublishSafe();
            }
        });
    }
}

void ProjectCloudExtension::MarkProjectSynced(bool success)
{
    auto& cloudDatabase = CloudProjectsDatabase::Get();

    const auto projectFilePath
        =audacity::ToUTF8(ProjectFileIO::Get(mProject).GetFileName());

    auto previousDbData = cloudDatabase.GetProjectDataForPath(projectFilePath);

    DBProjectData dbData;

    if (previousDbData) {
        dbData = *previousDbData;
    }

    dbData.LastModified = wxDateTime::Now().GetTicks();
    dbData.LastRead     = dbData.LastModified;
    dbData.SyncStatus   = success ? DBProjectData::SyncStatusSynced
                          : DBProjectData::SyncStatusUploading;

    cloudDatabase.UpdateProjectData(dbData);
}

ProjectCloudExtension::UploadQueueElement*
ProjectCloudExtension::UnsafeFindUploadQueueElement(
    const ProjectUploadOperation& uploadOperation)
{
    auto it = std::find_if(
        mUploadQueue.begin(), mUploadQueue.end(),
        [&](const auto& element)
    { return element->Operation.get() == &uploadOperation; });

    return it != mUploadQueue.end() ? it->get() : nullptr;
}

const ProjectCloudExtension::UploadQueueElement*
ProjectCloudExtension::UnsafeFindUploadQueueElement(
    const ProjectUploadOperation& uploadOperation) const
{
    return const_cast<ProjectCloudExtension*>(this)
           ->UnsafeFindUploadQueueElement(uploadOperation);
}

void ProjectCloudExtension::OnProjectPathChanged()
{
    bool wasCloudProject = false;

    {
        auto lock = std::lock_guard { mIdentifiersMutex };

        wasCloudProject = !mProjectId.empty();

        mProjectId.clear();
        mSnapshotId.clear();
    }

    // This will set the status to cloud if the project is a cloud project
    UpdateIdFromDatabase();

    if (mProjectId.empty() && wasCloudProject) {
        Publish({ AudiocomTrace::ignore, ProjectSyncStatus::Local }, false);
    }
}

std::string
ProjectCloudExtension::GetCloudProjectPage(AudiocomTrace trace) const
{
    auto& oauthService = GetOAuthService();
    auto& serviceConfig = GetServiceConfig();

    auto userId = audacity::ToUTF8(GetUserService().GetUserId());

    const auto projectId
        =ProjectCloudExtension::Get(mProject).GetCloudProjectId();

    const auto userSlug
        =CloudProjectsDatabase::Get().GetProjectUserSlug(projectId);

    auto projectPage = serviceConfig.GetProjectPagePath(userSlug, projectId, trace);
    auto url = oauthService.MakeAudioComAuthorizeURL(userId, projectPage);
    return url;
}

bool ProjectCloudExtension::IsBlockLocked(int64_t blockID) const
{
    // Try load the project info first based on the
    // file path
    const_cast<ProjectCloudExtension*>(this)->OnLoad();

    const auto projectId = GetCloudProjectId();

    if (projectId.empty()) {
        return false;
    }

    return CloudProjectsDatabase::Get().IsProjectBlockLocked(projectId, blockID);
}

ProjectSyncStatus ProjectCloudExtension::GetCurrentSyncStatus() const
{
    auto lock = std::lock_guard {
        const_cast<ProjectCloudExtension*>(this)->mStatusMutex
    };

    return mLastStatus.Status;
}

bool ProjectCloudExtension::IsFirstSyncDialogShown() const
{
    auto lock = std::lock_guard { mIdentifiersMutex };

    if (mProjectId.empty()) {
        return false;
    }

    return CloudProjectsDatabase::Get().IsFirstSyncDialogShown(mProjectId);
}

void ProjectCloudExtension::SetFirstSyncDialogShown(bool shown)
{
    auto lock = std::lock_guard { mIdentifiersMutex };

    if (mProjectId.empty()) {
        return;
    }

    CloudProjectsDatabase::Get().SetFirstSyncDialogShown(mProjectId, shown);
}

bool CloudStatusChangedMessage::IsSyncing() const noexcept
{
    return Status == ProjectSyncStatus::Syncing;
}
} // namespace audacity::cloud::audiocom::sync
