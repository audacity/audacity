/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSyncService.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "ProjectFileIOExtension.h"

#include <memory>
#include <vector>

#include "GlobalVariable.h"

namespace cloud::audiocom
{
namespace sync
{
class CloudProjectSnapshot;
class CloudSyncUI;
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
   struct CLOUD_AUDIOCOM_API UI
      : GlobalHook<UI, sync::CloudSyncUI&()>
   {};

   static CloudSyncService& Get();

   void SaveToCloud(AudacityProject& project);
private:
   void OnLoad(AudacityProject& project) override;
   bool OnSave(AudacityProject& project, bool fromTempProject) override;
   bool OnClose(AudacityProject& project) override;
   bool IsBlockLocked(const AudacityProject& project, int64_t blockId) const override;
   void OnUpdateSaved(AudacityProject& project, const ProjectSerializer& serializer) override;

   void CreateSnapshot(AudacityProject& project);

   std::vector<std::shared_ptr<sync::CloudProjectSnapshot>> mSnapshots;
};
} // namespace cloud::audiocom
