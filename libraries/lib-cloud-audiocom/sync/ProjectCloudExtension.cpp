/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectCloudExtension.h

  Dmitry Vedenko

**********************************************************************/

#include "ProjectCloudExtension.h"

#include <cstring>
#include <vector>
#include <unordered_map>

#include "CloudSyncUtils.h"

#include "CodeConversions.h"

#include "Project.h"
#include "XMLAttributeValueView.h"
#include "XMLWriter.h"

#include "ProjectFileIO.h"
#include "ProjectSerializer.h"

#include "MemoryX.h"

#include "CloudProjectsDatabase.h"

namespace cloud::audiocom::sync
{
namespace
{
const AttachedProjectObjects::RegisteredFactory key {
   [](AudacityProject& project)
   { return std::make_shared<ProjectCloudExtension>(project); }
};
} // namespace


ProjectCloudExtension::ProjectCloudExtension(AudacityProject& project)
    : mProject { project }
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

void ProjectCloudExtension::MarkPendingCloudSave()
{
   mPendingCloudSave = true;
}

void ProjectCloudExtension::OnLoad()
{
   if (!IsCloudProject())
      UpdateIdFromDatabase();
}

void ProjectCloudExtension::OnSnapshotCreated(
   std::string_view projectId, std::string_view snapshotId)
{
   auto& cloudDatabase = CloudProjectsDatabase::Get();

   const auto projectFilePath =
      audacity::ToUTF8(ProjectFileIO::Get(mProject).GetFileName());

   auto previosDbData = cloudDatabase.GetProjectDataForPath(projectFilePath);

   DBProjectData dbData;

   if (previosDbData)
      dbData = *previosDbData;

   dbData.ProjectId = projectId;
   dbData.SnapshotId = snapshotId;
   dbData.LocalPath = projectFilePath;
   dbData.LastModified = wxDateTime::Now().GetTicks();
   dbData.LastRead = dbData.LastModified;
   dbData.SyncStatus = DBProjectData::SyncStatusUploading;
   dbData.SavesCount++;

   cloudDatabase.UpdateProjectData(dbData);

   mProjectId = projectId;
   mSnapshotId = snapshotId;
}

void ProjectCloudExtension::OnSnapshotSynced(
   std::string_view projectId, std::string_view snapshotId)
{
   mProjectId = projectId;
   mSnapshotId = snapshotId;
}

void ProjectCloudExtension::OnSyncCompleted(bool successful)
{
   mPendingCloudSave = !CloudProjectsDatabase::Get().MarkProjectAsSynced(mProjectId, mSnapshotId);
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
   if (!IsCloudProject() && !mPendingCloudSave)
      return false;

   const size_t dictSize = serializer.GetDict().GetSize();
   const size_t projectSize = serializer.GetData().GetSize();

   mUpdatedProjectContents.resize(projectSize + dictSize + sizeof(uint64_t));

   const uint64_t dictSizeData =
      IsLittleEndian() ? dictSize : SwapIntBytes(dictSize);

   std::memcpy(mUpdatedProjectContents.data(), &dictSizeData, sizeof(uint64_t));

   uint64_t offset = sizeof(dictSize);

   for (const auto [chunkData, size] : serializer.GetDict())
   {
      std::memcpy(mUpdatedProjectContents.data() + offset, chunkData, size);
      offset += size;
   }

   for (const auto [chunkData, size] : serializer.GetData())
   {
      std::memcpy(mUpdatedProjectContents.data() + offset, chunkData, size);
      offset += size;
   }

   return true;
}

std::weak_ptr<AudacityProject> ProjectCloudExtension::GetProject() const
{
   return mProject.weak_from_this();
}

const std::vector<uint8_t>& ProjectCloudExtension::GetUpdatedProjectContents() const
{
   return mUpdatedProjectContents;
}

void ProjectCloudExtension::UpdateIdFromDatabase()
{
   auto& projectFileIO = ProjectFileIO::Get(mProject);
   auto& cloudDatabase = CloudProjectsDatabase::Get();

   auto projectData = cloudDatabase.GetProjectDataForPath(
      audacity::ToUTF8(projectFileIO.GetFileName()));

   if (!projectData)
      return;

   mProjectId = projectData->ProjectId;
   mSnapshotId = projectData->SnapshotId;
}

} // namespace cloud::audiocom::sync
