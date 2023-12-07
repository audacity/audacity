/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectCloudExtension.h

  Dmitry Vedenko

**********************************************************************/

#include "ProjectCloudExtension.h"

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

namespace cloud::audiocom::sync
{
namespace
{
const AttachedProjectObjects::RegisteredFactory key {
   [](AudacityProject& project)
   { return std::make_shared<ProjectCloudExtension>(project); }
};

constexpr auto XmlAttrProjectId = "audiocom_project_id";
constexpr auto XmlAttrSnapshotId = "audiocom_snapshot_id";

ProjectFileIORegistry::AttributeWriterEntry xmlWriter {
   [](const AudacityProject& project, XMLWriter& xmlFile)
   {
      auto& formats = ProjectCloudExtension::Get(project);

      if (formats.IsCloudProject())
      {
         xmlFile.WriteAttr(
            XmlAttrProjectId,
            audacity::ToWXString(formats.GetCloudProjectId()));
         xmlFile.WriteAttr(
            XmlAttrSnapshotId, audacity::ToWXString(formats.GetSnapshotId()));
      }  
   }
};

ProjectFileIORegistry::AttributeReaderEntries xmlReaders {
   (ProjectCloudExtension& (*)(AudacityProject&)) &ProjectCloudExtension::Get,
   {
      { XmlAttrProjectId,
        [](auto& cloudExtension, auto value)
        { cloudExtension.SetCloudProjectId(value.Get(std::string_view {}));
        } },
      { XmlAttrSnapshotId, [](auto& cloudExtension, auto value)
        {
           cloudExtension.SetSnapshotId(
              value.Get(std::string_view {}));
        } },
   }
};
} // namespace


ProjectCloudExtension::ProjectCloudExtension(AudacityProject& project)
    : mProject { project }
{
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

std::string_view ProjectCloudExtension::GetCloudProjectId() const
{
   return mProjectId;
}

void ProjectCloudExtension::SetCloudProjectId(std::string_view projectId)
{
   mProjectId = projectId;
}

std::string_view ProjectCloudExtension::GetSnapshotId() const
{
   return mSnapshotId;
}

void ProjectCloudExtension::SetSnapshotId(std::string_view snapshotId)
{
   mSnapshotId = snapshotId;
}

std::weak_ptr<AudacityProject> ProjectCloudExtension::GetProject() const
{
   return mProject.weak_from_this();
}

std::vector<uint8_t> ProjectCloudExtension::GetUpdatedProjectContents() const
{
   ProjectSerializer serializer;

   auto& projectFileIO = ProjectFileIO::Get(mProject);

   projectFileIO.SerializeProject(serializer);
   projectFileIO.UpdateSaved(serializer);

   const size_t dictSize = serializer.GetDict().GetSize();
   const size_t projectSize = serializer.GetData().GetSize();
      

   std::vector<uint8_t> data;

   data.resize(projectSize + dictSize + sizeof(uint64_t));

   const uint64_t dictSizeData = IsLittleEndian() ?
                                dictSize :
                                SwapIntBytes(dictSize);

   std::memcpy(data.data(), &dictSizeData, sizeof(uint64_t));

   uint64_t offset = sizeof(dictSize);

   for (const auto [chunkData, size] : serializer.GetDict())
   {
      std::memcpy(data.data() + offset, chunkData, size);
      offset += size;
   }

   for (const auto [chunkData, size] : serializer.GetData ())
   {
      std::memcpy(data.data() + offset, chunkData, size);
      offset += size;
   }

   return data;
}

} // namespace cloud::audiocom::sync
