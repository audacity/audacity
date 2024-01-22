/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectExtension.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <functional>
#include <memory>
#include <string>
#include <string_view>
#include <vector>
#include <unordered_map>

#include "ClientData.h"
#include "Observer.h"

class AudacityProject;
class ProjectSerializer;

namespace cloud::audiocom::sync
{
struct CloudStatusChanged final
{
   bool IsSyncing { false };
   bool LastSyncSuccessful { false };
   bool IsCloudProject { false };
};

class CLOUD_AUDIOCOM_API ProjectCloudExtension final :
    public ClientData::Base,
    public Observer::Publisher<CloudStatusChanged>
{
public:
   explicit ProjectCloudExtension(AudacityProject& project);
   ~ProjectCloudExtension() override;

   static ProjectCloudExtension& Get(AudacityProject& project);
   static const ProjectCloudExtension& Get(const AudacityProject& project);

   bool IsCloudProject() const;

   void MarkPendingCloudSave();

   void OnLoad();

   void OnSnapshotCreated(std::string_view projectId, std::string_view snapshotId);

   void OnSnapshotSynced(std::string_view projectId, std::string_view snapshotId);

   void OnSyncCompleted(bool successful);

   std::string_view GetCloudProjectId() const;
   std::string_view GetSnapshotId() const;

   bool OnUpdateSaved(const ProjectSerializer& serializer);

   std::weak_ptr<AudacityProject> GetProject() const;
   const std::vector<uint8_t>& GetUpdatedProjectContents() const;

   void SuppressAutoDownload();

   bool GetAutoDownloadSuppressed() const;

   bool NeedsMixdownSync() const;
   void MixdownSynced();

private:
   void UpdateIdFromDatabase();

   AudacityProject& mProject;

   std::string mProjectId;
   std::string mSnapshotId;

   std::vector<uint8_t> mUpdatedProjectContents;

   bool mPendingCloudSave { false };
   bool mSuppressAutoDownload { false };
};
} // namespace cloud::audiocom::sync
