/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSyncService.cpp

  Dmitry Vedenko

**********************************************************************/
#include "CloudSyncService.h"

#include <algorithm>

#include "sync/CloudProjectSnapshot.h"
#include "sync/ProjectCloudExtension.h"

#include "ServiceConfig.h"
#include "OAuthService.h"

#include "sqlite3.h"

#include "ProjectFileIO.h"
#include "DBConnection.h"

#include "BasicUI.h"

#include <wx/log.h>


namespace cloud::audiocom
{
CloudSyncService& CloudSyncService::Get()
{
   static CloudSyncService service;
   return service;
}

void CloudSyncService::SaveToCloud(AudacityProject& project)
{
}

void CloudSyncService::OnLoad(AudacityProject& project)
{
}

void CloudSyncService::OnSave(AudacityProject& project)
{
   auto& cloudExtension = sync::ProjectCloudExtension::Get(project);

   mSnapshots.emplace_back(sync::CloudProjectSnapshot::Create(
      GetServiceConfig(), GetOAuthService(), cloudExtension, [](const auto& update) {
         wxLogDebug(
            "Update: %lld/%lld\n\tproject uploaded: %d\n\tcompleted: %d\n\tsuccess: %d\n\t%s", update.SampleBlocksUploaded,
            update.SampleBlocksCount, update.ProjectBlobUploaded,
            update.Completed, update.Successful, update.ErrorMessage);

   }));
}

bool CloudSyncService::OnClose(AudacityProject& project)
{
   return true;
}

bool CloudSyncService::IsBlockLocked(
   const AudacityProject& project, int64_t blockId) const
{
   return false;
}

namespace
{
ProjectFileIOExtensionRegistry::Extension extension { CloudSyncService::Get() };
}
} // namespace cloud::audiocom
