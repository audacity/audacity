/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSyncService.cpp

  Dmitry Vedenko

**********************************************************************/
#include "CloudSyncService.h"

#include <algorithm>

#include "sync/CloudProjectSnapshot.h"

#include "sqlite3.h"

#include "ProjectFileIO.h"
#include "DBConnection.h"

#include "BasicUI.h"

namespace
{
// SQL Query to create the audio com cloud sync related table
const char* createTableQuery = R"(
CREATE TABLE IF NOT EXISTS audiocom_cloud_sync
(
   project_id INTEGER,
   version INTEGER,
   last_audio_preview_version INTEGER,
   last_image_preview_version INTEGER,
   PRIMARY KEY (project_id)
);

CREATE TABLE IF NOT EXISTS audio_com_pending_blocks
(
   project_id INTEGER,
   block_id INTEGER,
   PRIMARY KEY (project_id, block_id)
);

CREATE TABLE IF NOT EXISTS audio_com_missing_blocks
(
   project_id INTEGER,
   block_id INTEGER,
   upload_url TEXT,
   confirm_url TEXT,
   PRIMARY KEY (project_id, block_id)
);
)";



} // namespace
namespace cloud::audiocom
{
CloudSyncService& CloudSyncService::Get()
{
   static CloudSyncService service;
   return service;
}

void CloudSyncService::OnLoad(AudacityProject& project)
{
}

void CloudSyncService::OnSave(
   AudacityProject& project)
{
   auto db = ProjectFileIO::Get(project).GetConnection().DB();

   // Try to create the tables required for the cloud sync service
   int rc = sqlite3_exec(db, createTableQuery, nullptr, nullptr, nullptr);

   if (rc != SQLITE_OK)
      return;

   auto projectSnapshot =
      std::make_unique<sync::CloudProjectSnapshot>(project);

   mSnapshots.push_back(std::move(projectSnapshot));

   auto snapshotPtr = mSnapshots.back().get();

   snapshotPtr->SaveSnapshot(
      [this, snapshotPtr](const sync::SnapshotOperationResult& result)
      {
         BasicUI::CallAfter(
            [this, snapshotPtr]()
            {
               mSnapshots.erase(
                  std::remove_if(
                     mSnapshots.begin(), mSnapshots.end(),
                     [snapshotPtr](const auto& v)
                     { return v.get() == snapshotPtr; }),
                  mSnapshots.end());
            });
      });
}

bool CloudSyncService::OnClose(AudacityProject& project)
{
   return true;
}

namespace
{
ProjectFileIOExtensionRegistry::Extension extension { CloudSyncService::Get() };
}
} // namespace cloud::audiocom
