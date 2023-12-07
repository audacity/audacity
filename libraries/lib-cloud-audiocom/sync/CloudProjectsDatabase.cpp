/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectsDatabase.cpp

  Dmitry Vedenko

**********************************************************************/
#include "CloudProjectsDatabase.h"

#include "AppEvents.h"
#include "CodeConversions.h"
#include "FileNames.h"

namespace cloud::audiocom::sync
{
namespace
{
const auto createTableQuery = R"(
CREATE TABLE IF NOT EXISTS projects
(
   project_id INTEGER,
   snapshot_id TEXT,
   saves_count INTEGER,
   last_audio_preview_save INTEGER,
   path TEXT,
   last_modified INTEGER,
   last_read INTEGER,
   sync_status INTEGER,
   PRIMARY KEY (project_id)
);

CREATE TABLE IF NOT EXISTS pending_blocks
(
   project_id INTEGER,
   block_id INTEGER,
   PRIMARY KEY (project_id, block_id)
);

CREATE TABLE IF NOT EXISTS block_hashes
(
   project_id INTEGER,
   block_id INTEGER,
   hash TEXT,
   PRIMARY KEY (project_id, block_id)
);

CREATE INDEX IF NOT EXISTS block_hashes_index ON block_hashes (hash);
)";
}

CloudProjectsDatabase::CloudProjectsDatabase()
{
   AppEvents::OnAppInitialized(
      [this]
      {
         const auto configDir = FileNames::ConfigDir();
         const auto configPath = configDir + "/audiocom_sync.db";

         auto db = sqlite::Connection::Open(audacity::ToUTF8(configPath));

         if (!db)
            return;

         mConnection = std::move(*db);

         auto result = mConnection.Execute(createTableQuery);

         if (!result)
         {
            mConnection = {};
            return;
         }
      });
}

CloudProjectsDatabase& CloudProjectsDatabase::Get()
{
   static CloudProjectsDatabase instance;
   return instance;
}

bool CloudProjectsDatabase::IsOpen() const
{
   return mConnection.IsOpen();
}
sqlite::Connection& CloudProjectsDatabase::GetConnection()
{
   return mConnection;
}

const sqlite::Connection& CloudProjectsDatabase::GetConnection() const
{
   return mConnection;
}
} // namespace cloud::audiocom::sync
