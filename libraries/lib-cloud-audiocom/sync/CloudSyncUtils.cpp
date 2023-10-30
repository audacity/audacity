/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSyncUtils.cpp

  Dmitry Vedenko

**********************************************************************/
#include "CloudSyncUtils.h"

#include <numeric>
#include <set>

#include <rapidjson/document.h>
#include <rapidjson/writer.h>
#include <rapidjson/stringbuffer.h>
#include <rapidjson/reader.h>

#include "sqlite3.h"

#include "Track.h"
#include "WaveTrack.h"
#include "Sequence.h"
#include "SampleBlock.h"
#include "ProjectFileIO.h"
#include "DBConnection.h"

namespace cloud::audiocom::sync
{
static_assert(sizeof(SampleBlockID) == sizeof(BlockID));
static_assert(std::is_signed_v<SampleBlockID> == std::is_signed_v<BlockID>);

SampleBlockIDs GetProjectBlocks(const AudacityProject& project)
{
   std::set<BlockID> result;

   auto& tracks = TrackList::Get(project);

   for (auto track : tracks.Any<const WaveTrack>())
   {
      for (auto interval : track->Intervals())
      {
         for (auto channel : interval->Channels())
         {
            auto& sequence = channel->GetSequence();
            auto& blockArray = sequence.GetBlockArray();

            for (auto& block : blockArray)
               result.insert(block.sb->GetBlockID());
         }
      }
   }

   return SampleBlockIDs(result.begin(), result.end());
}

std::string SerializeBlockIDs(const SampleBlockIDs& ids)
{
   using namespace rapidjson;

   Document document;
   document.SetArray();

   for (const auto& id : ids)
   {
      document.PushBack(id, document.GetAllocator());
   }

   StringBuffer buffer;
   Writer<StringBuffer> writer(buffer);
   document.Accept(writer);

   return buffer.GetString();
}

std::optional<ProjectResponse> DeserializeProjectResponse(const std::string& data)
{
   using namespace rapidjson;

   Document document;
   document.Parse(data.c_str());

   if (!document.IsObject())
      return {};

   ProjectResponse result;

   if (!document.HasMember("project_id") || !document["project_id"].IsInt64())
      return {};

   result.ProjectId = document["project_id"].GetInt64();

   if (!document.HasMember("version") || !document["version"].IsInt64())
      return {};

   result.Version = document["version"].GetInt64();

   if (!document.HasMember("missing_blocks") || !document["missing_blocks"].IsArray())
      return {};

   for (const auto& value : document["missing_blocks"].GetArray())
   {
      if (!value.IsObject())
         return {};

      if (!value.HasMember("block_id") || !value["block_id"].IsInt64())
         return {};

      if (!value.HasMember("upload_url") || !value["upload_url"].IsString())
         return {};

      if (!value.HasMember("confirm_url") || !value["confirm_url"].IsString())
         return {};

      result.MissingBlocks.push_back(MissingBlock {
         value["block_id"].GetInt64(), value["upload_url"].GetString(),
         value["confirm_url"].GetString() });
   }

   return result;
}

sqlite3* GetProjectDatabaseHandle(AudacityProject& project)
{
   auto& io = ProjectFileIO::Get(project);
   auto& db = io.GetConnection();
   return db.DB();
}

bool CheckTableExists(AudacityProject& project, std::string_view tableName)
{
   auto db = GetProjectDatabaseHandle(project);

   sqlite3_stmt* stmt = nullptr;

   int rc = sqlite3_prepare_v2(
      db,
      "SELECT EXISTS(SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ?)",
      -1, &stmt, nullptr);

   if (rc != SQLITE_OK)
      return false;

   auto cleanup = finally([&stmt] { sqlite3_finalize(stmt); });

   sqlite3_bind_text(stmt, 1, tableName.data(), tableName.size(), nullptr);

   rc = sqlite3_step(stmt);

   if (rc != SQLITE_ROW)
      return false;

   return sqlite3_column_int(stmt, 0) == 1;
}


std::vector<MissingBlock> GetMissingBlocks(AudacityProject& project)
{
   if (!CheckTableExists(project, "audio_com_missing_blocks"))
      return {};

   auto db = GetProjectDatabaseHandle(project);

   sqlite3_stmt* stmt = nullptr;

   int rc = sqlite3_prepare_v2 (
      db,
      "SELECT block_id, upload_url, confirm_url FROM audio_com_missing_blocks",
      -1, &stmt, nullptr);

   if (rc != SQLITE_OK)
      return {};

   auto cleanup = finally([&stmt] { sqlite3_finalize(stmt); });

   std::vector<MissingBlock> result;

   rc = sqlite3_step(stmt);

   while (rc == SQLITE_ROW)
   {
      result.push_back (MissingBlock{
         sqlite3_column_int64(stmt, 0),
         reinterpret_cast<const char*>(sqlite3_column_text(stmt, 1)),
         reinterpret_cast<const char*>(sqlite3_column_text(stmt, 2)) });

      rc = sqlite3_step(stmt);
   }

   return result;
}

bool AppendMissingBlocks(
   AudacityProject& project, const std::vector<MissingBlock>& blocks)
{
   auto db = GetProjectDatabaseHandle(project);

   sqlite3_stmt* stmt = nullptr;

   int rc = sqlite3_prepare_v2 (
      db,
      "INSERT OR REPLACE INTO audio_com_missing_blocks (block_id, upload_url, confirm_url) VALUES (?, ?, ?)",
      -1, &stmt, nullptr);

   if (rc != SQLITE_OK)
      return false;

   auto cleanup = finally([&stmt] { sqlite3_finalize(stmt); });

   for (const auto& block : blocks)
   {
      sqlite3_bind_int64(stmt, 1, block.Id);
      sqlite3_bind_text(stmt, 2, block.UploadUrl.data(), block.UploadUrl.size(), nullptr);
      sqlite3_bind_text(stmt, 3, block.ConfirmUrl.data(), block.ConfirmUrl.size(), nullptr);

      rc = sqlite3_step(stmt);

      if (rc != SQLITE_DONE)
         return false;

      sqlite3_reset(stmt);
   }

   return true;
}

void RemoveMissingBlock(AudacityProject& project, BlockID blockId)
{
   auto db = GetProjectDatabaseHandle(project);

   sqlite3_stmt* stmt = nullptr;

   int rc = sqlite3_prepare_v2(
      db, "DELETE FROM audio_com_missing_blocks WHERE block_id = ?", -1, &stmt,
      nullptr);

   if (rc != SQLITE_OK)
      return;

   auto cleanup = finally([&stmt] { sqlite3_finalize(stmt); });

   sqlite3_bind_int64(stmt, 1, blockId);

   rc = sqlite3_step(stmt);

   if (rc != SQLITE_DONE)
      return;

   sqlite3_reset(stmt);
}

} // namespace cloud::audiocom::sync
