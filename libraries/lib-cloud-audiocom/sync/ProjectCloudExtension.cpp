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

#include "AsynchronousOperation.h"

#include "CloudSettings.h"
#include "CloudSyncUtils.h"

#include "CodeConversions.h"

#include "Project.h"

#include "ProjectFileIO.h"
#include "ProjectSerializer.h"

#include "BasicUI.h"

#include "MemoryX.h"

#include "CloudProjectsDatabase.h"

#include "Track.h"

namespace cloud::audiocom::sync
{
namespace
{
const AttachedProjectObjects::RegisteredFactory key {
   [](AudacityProject& project)
   { return std::make_shared<ProjectCloudExtension>(project); }
};
} // namespace

struct ProjectCloudExtension::UploadQueueElement final
{
   ProjectUploadData Data;
   std::shared_ptr<ProjectUploadOperation> Operation;

   int64_t BlocksHandled { 0 };
   int64_t BlocksTotal { 0 };

   bool SnapshotCreated { false };
   bool ProjectDataUploaded { false };
   bool ReadyForUpload { false };
   bool Synced { false };
};

struct ProjectCloudExtension::CloudStatusChangedNotifier final :
    Observer::Publisher<CloudStatusChangedMessage>
{
   using Observer::Publisher<CloudStatusChangedMessage>::Publish;
};

ProjectCloudExtension::ProjectCloudExtension(AudacityProject& project)
    : mProject { project }
    , mAsyncStateNotifier { std::make_unique<CloudStatusChangedNotifier>() }
    , mUIStateNotifier { std::make_unique<CloudStatusChangedNotifier>() }
{
   if (!ProjectFileIO::Get(project).IsTemporary())
      UpdateIdFromDatabase();
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
   return !mProjectId.empty();
}

void ProjectCloudExtension::OnLoad()
{
   if (IsCloudProject())
      return;

   UpdateIdFromDatabase();
}

void ProjectCloudExtension::OnSyncStarted()
{
   mPendingCloudSave = false;

   if (!mNeedsMixdownSync && !IsCloudProject())
      mNeedsMixdownSync = true;

   auto element = std::make_shared<UploadQueueElement>();

   element->Data.Tracks = TrackList::Create(nullptr);

   for (auto pTrack : TrackList::Get(mProject))
   {
      if (pTrack->GetId() == TrackId {})
         // Don't copy a pending added track
         continue;
      element->Data.Tracks->Append(std::move(*pTrack->Duplicate()));
   }

   auto lock = std::lock_guard { mUploadQueueMutex };
   mUploadQueue.push_back(std::move(element));
}

void ProjectCloudExtension::OnUploadOperationCreated(
   std::shared_ptr<ProjectUploadOperation> uploadOperation)
{
   auto lock = std::lock_guard { mUploadQueueMutex };
   assert(mUploadQueue.size() > 0);

   if (mUploadQueue.empty())
      return;

   mUploadQueue.back()->Operation = uploadOperation;
   UnsafeUpdateProgress();
}

void ProjectCloudExtension::OnBlocksHashed(
   ProjectUploadOperation& uploadOperation)
{
   auto lock    = std::lock_guard { mUploadQueueMutex };
   auto element = UnsafeFindUploadQueueElement(uploadOperation);

   if (!element)
      return;

   element->ReadyForUpload = true;

   for (auto& operation : mUploadQueue)
   {
      // It is safe to start the upload right away
      if (operation.get() == element)
      {
         uploadOperation.Start(element->Data);
         return;
      }
      // There is a pending operation, wait for it to finish
      if (!operation->ProjectDataUploaded)
         return;
   }
}

void ProjectCloudExtension::OnSnapshotCreated(
   const ProjectUploadOperation& uploadOperation,
   const CreateSnapshotResponse& response)
{
   auto& cloudDatabase = CloudProjectsDatabase::Get();

   const auto projectFilePath =
      audacity::ToUTF8(ProjectFileIO::Get(mProject).GetFileName());

   auto previosDbData = cloudDatabase.GetProjectDataForPath(projectFilePath);

   DBProjectData dbData;

   if (previosDbData)
      dbData = *previosDbData;

   dbData.ProjectId    = response.Project.Id;
   dbData.SnapshotId   = response.Snapshot.Id;
   dbData.LocalPath    = projectFilePath;
   dbData.LastModified = wxDateTime::Now().GetTicks();
   dbData.LastRead     = dbData.LastModified;
   dbData.SyncStatus   = DBProjectData::SyncStatusUploading;
   dbData.SavesCount++;

   cloudDatabase.UpdateProjectData(dbData);

   mProjectId  = response.Project.Id;
   mSnapshotId = response.Snapshot.Id;

   auto lock    = std::lock_guard { mUploadQueueMutex };
   auto element = UnsafeFindUploadQueueElement(uploadOperation);

   if (!element)
      return;

   element->SnapshotCreated = true;
   element->BlocksTotal     = response.SyncState.MissingBlocks.size();

   for (auto& operation : mUploadQueue)
   {
      if (operation->SnapshotCreated)
         continue;

      if (!operation->ReadyForUpload)
         return;

      operation->Operation->Start(operation->Data);
   }

   UnsafeUpdateProgress();
}

void ProjectCloudExtension::OnProjectDataUploaded(
   const ProjectUploadOperation& uploadOperation)
{
   auto lock    = std::lock_guard { mUploadQueueMutex };
   auto element = UnsafeFindUploadQueueElement(uploadOperation);

   if (!element)
      return;

   element->ProjectDataUploaded = true;

   UnsafeUpdateProgress();
}

void ProjectCloudExtension::OnBlockUploaded(
   const ProjectUploadOperation& uploadOperation, std::string_view blockID,
   bool successful)
{
   auto lock    = std::lock_guard { mUploadQueueMutex };
   auto element = UnsafeFindUploadQueueElement(uploadOperation);

   if (!element)
      return;

   element->BlocksHandled++;

   UnsafeUpdateProgress();
}

void ProjectCloudExtension::OnSyncCompleted(
   const ProjectUploadOperation* uploadOperation,
   std::optional<CloudSyncError> error)
{
   auto lock = std::lock_guard { mUploadQueueMutex };

   if (uploadOperation == nullptr)
   {
      mUploadQueue.pop_back();
      return;
   }

   auto element = UnsafeFindUploadQueueElement(*uploadOperation);

   if (element != nullptr)
      element->Synced = true;

   if (!std::all_of(
          mUploadQueue.begin(), mUploadQueue.end(),
          [](auto& operation) { return operation->Synced; }))
   {
      UnsafeUpdateProgress();
      return;
   }

   mUploadQueue.clear();

   MarkProjectSynced(!error.has_value());

   Publish({ error.has_value() ? ProjectSyncStatus::Failed :
                                 ProjectSyncStatus::Synced,
             {},
             error });
}

void ProjectCloudExtension::CancelSync()
{
}

bool ProjectCloudExtension::IsSyncing() const
{
   auto lock = std::lock_guard {
      const_cast<ProjectCloudExtension*>(this)->mStatusMutex
   };

   return mLastStatus.Status == ProjectSyncStatus::Syncing;
}

std::string_view ProjectCloudExtension::GetCloudProjectId() const
{
   return mProjectId;
}

std::string_view ProjectCloudExtension::GetSnapshotId() const
{
   return mSnapshotId;
}

bool ProjectCloudExtension::OnUpdateSaved(const ProjectSerializer& serializer)
{
   auto lock = std::lock_guard { mUploadQueueMutex };

   if (mUploadQueue.empty())
      return false;

   const size_t dictSize    = serializer.GetDict().GetSize();
   const size_t projectSize = serializer.GetData().GetSize();

   std::vector<uint8_t> data;
   data.resize(projectSize + dictSize + sizeof(uint64_t));

   const uint64_t dictSizeData =
      IsLittleEndian() ? dictSize : SwapIntBytes(dictSize);

   std::memcpy(data.data(), &dictSizeData, sizeof(uint64_t));

   uint64_t offset = sizeof(dictSize);

   for (const auto [chunkData, size] : serializer.GetDict())
   {
      std::memcpy(data.data() + offset, chunkData, size);
      offset += size;
   }

   for (const auto [chunkData, size] : serializer.GetData())
   {
      std::memcpy(data.data() + offset, chunkData, size);
      offset += size;
   }

   mUploadQueue.back()->Data.ProjectSnapshot = std::move(data);

   return true;
}

std::weak_ptr<AudacityProject> ProjectCloudExtension::GetProject() const
{
   return mProject.weak_from_this();
}

void ProjectCloudExtension::SuppressAutoDownload()
{
   mSuppressAutoDownload = true;
}

bool ProjectCloudExtension::GetAutoDownloadSuppressed() const
{
   return mSuppressAutoDownload;
}

void ProjectCloudExtension::MarkNeedsMixdownSync()
{
   mNeedsMixdownSync = true;
}

bool ProjectCloudExtension::NeedsMixdownSync() const
{
   if (mNeedsMixdownSync)
      return true;

   if (!IsCloudProject())
      return false;

   auto& cloudDatabase = CloudProjectsDatabase::Get();
   auto dbData         = cloudDatabase.GetProjectData(mProjectId);

   if (!dbData)
      return false;

   if (dbData->LastAudioPreview == 0)
      return true;

   const auto frequency = MixdownGenerationFrequency.Read();

   if (frequency == 0)
      return false;

   const auto savesSinceLastMixdown =
      dbData->SavesCount - dbData->LastAudioPreview;

   return savesSinceLastMixdown >= frequency;
}

void ProjectCloudExtension::MixdownSynced()
{
   if (!IsCloudProject())
      return;

   mNeedsMixdownSync = false;

   auto& cloudDatabase = CloudProjectsDatabase::Get();
   auto dbData         = cloudDatabase.GetProjectData(mProjectId);

   if (!dbData)
      return;

   dbData->LastAudioPreview = dbData->SavesCount;
   cloudDatabase.UpdateProjectData(*dbData);
}

int64_t ProjectCloudExtension::GetSavesCount() const
{
   if (!IsCloudProject())
      return 0;

   auto& cloudDatabase = CloudProjectsDatabase::Get();
   auto dbData         = cloudDatabase.GetProjectData(mProjectId);

   if (!dbData)
      return 0;

   return dbData->SavesCount;
}

int64_t ProjectCloudExtension::GetSavesCountSinceMixdown() const
{
   if (!IsCloudProject())
      return 0;

   auto& cloudDatabase = CloudProjectsDatabase::Get();
   auto dbData         = cloudDatabase.GetProjectData(mProjectId);

   if (!dbData)
      return 0;

   return dbData->SavesCount - dbData->LastAudioPreview;
}

Observer::Subscription ProjectCloudExtension::SubscribeStatusChanged(
   std::function<void(const CloudStatusChangedMessage&)> callback,
   bool onUIThread)
{
   return onUIThread ? mUIStateNotifier->Subscribe(std::move(callback)) :
                       mAsyncStateNotifier->Subscribe(std::move(callback));
}

void ProjectCloudExtension::UpdateIdFromDatabase()
{
   auto& projectFileIO = ProjectFileIO::Get(mProject);
   auto& cloudDatabase = CloudProjectsDatabase::Get();

   auto projectData = cloudDatabase.GetProjectDataForPath(
      audacity::ToUTF8(projectFileIO.GetFileName()));

   if (!projectData)
      return;

   mProjectId  = projectData->ProjectId;
   mSnapshotId = projectData->SnapshotId;

   Publish({
      projectData->SyncStatus ==
            DBProjectData::SyncStatusType::SyncStatusSynced ?
         ProjectSyncStatus::Synced :
         ProjectSyncStatus::Unsynced,
   });
}

void ProjectCloudExtension::UnsafeUpdateProgress()
{
   if (mUploadQueue.empty())
      return;

   int64_t handledElements = 0;
   int64_t totalElements   = 0;

   for (auto& element : mUploadQueue)
   {
      handledElements +=
         element->BlocksHandled + int(element->ProjectDataUploaded);
      totalElements += element->BlocksTotal + 1;
   }

   assert(totalElements > 0);

   Publish({ ProjectSyncStatus::Syncing,
             double(handledElements) / double(totalElements) });
}

void ProjectCloudExtension::Publish(CloudStatusChangedMessage cloudStatus)
{
   {
      auto lock   = std::lock_guard { mStatusMutex };
      mLastStatus = cloudStatus;
   }

   mAsyncStateNotifier->Publish(cloudStatus);

   if (BasicUI::IsUiThread())
   {
      mUINotificationPending.store(false);
      mUIStateNotifier->Publish(cloudStatus);
   }
   else if (!mUINotificationPending.exchange(true))
   {
      BasicUI::CallAfter(
         [this]
         {
            if (mUINotificationPending.exchange(false))
            {
               auto lock = std::lock_guard { mStatusMutex };
               mUIStateNotifier->Publish(mLastStatus);
            }
         });
   }
}

void ProjectCloudExtension::MarkProjectSynced(bool success)
{
   auto& cloudDatabase = CloudProjectsDatabase::Get();

   const auto projectFilePath =
      audacity::ToUTF8(ProjectFileIO::Get(mProject).GetFileName());

   auto previosDbData = cloudDatabase.GetProjectDataForPath(projectFilePath);

   DBProjectData dbData;

   if (previosDbData)
      dbData = *previosDbData;

   dbData.LastModified = wxDateTime::Now().GetTicks();
   dbData.LastRead     = dbData.LastModified;
   dbData.SyncStatus   = success ? DBProjectData::SyncStatusSynced :
                                   DBProjectData::SyncStatusUploading;

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

void ProjectCloudExtension::MarkPendingCloudSave()
{
   mPendingCloudSave = true;
}

bool ProjectCloudExtension::IsPendingCloudSave() const
{
   return mPendingCloudSave;
}

bool CloudStatusChangedMessage::IsSyncing() const noexcept
{
   return Status == ProjectSyncStatus::Syncing;
}

} // namespace cloud::audiocom::sync
