/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectSnapshot.cpp

  Dmitry Vedenko

**********************************************************************/
#include "CloudProjectSnapshot.h"

#include <algorithm>

#include <wx/log.h>

#include "sqlite3.h"

#include "MemoryX.h"

#include "Project.h"
#include "TransactionScope.h"

#include "CodeConversions.h"

#include "Request.h"
#include "IResponse.h"
#include "NetworkManager.h"
#include "MultipartData.h"

#include "MissingBlocksUploader.h"


namespace cloud::audiocom::sync
{
namespace
{
const char* DeleteBlocksQuery =
   "DELETE FROM audio_com_pending_blocks WHERE project_id = ? AND block_id = ?";

const char* InsertBlocksQuery =
   "INSERT INTO audio_com_pending_blocks VALUES (?, ?)";


std::pair<int64_t, int64_t> GetProjectInfo(AudacityProject& project)
{
   constexpr std::pair<int64_t, int64_t> unassignedProject = { UNASSIGNED_PROJECT_ID, 0LL };

   if (!CheckTableExists(project, "audiocom_cloud_sync"))
      return unassignedProject;

   auto db = GetProjectDatabaseHandle(project);

   sqlite3_stmt* stmt = nullptr;

   int rc = sqlite3_prepare_v2(
      db, "SELECT project_id, version FROM audiocom_cloud_sync", -1, &stmt, nullptr);

   if (rc != SQLITE_OK)
      return unassignedProject;

   auto cleanup = finally([&stmt] { sqlite3_finalize(stmt); });

   rc = sqlite3_step(stmt);

   if (rc != SQLITE_ROW)
      return unassignedProject;

   return { sqlite3_column_int64(stmt, 0), sqlite3_column_int64(stmt, 1) };
}

void UpdateProjectInfo(AudacityProject& project, int64_t projectId, int64_t version)
{
   TransactionScope transaction(project, "CloudSaveUpdateProjectInfo");

   auto db = GetProjectDatabaseHandle(project);

   sqlite3_stmt* stmt = nullptr;

   int rc = sqlite3_prepare_v2 (
      db, "INSERT OR REPLACE INTO audiocom_cloud_sync VALUES (?, ?, -1, -1)", -1, &stmt, nullptr);

   if (rc != SQLITE_OK)
      return;

   auto cleanup = finally([&stmt] { sqlite3_finalize(stmt); });

   sqlite3_bind_int64(stmt, 1, projectId);
   sqlite3_bind_int64(stmt, 2, version);

   rc = sqlite3_step(stmt);

   if (rc != SQLITE_DONE)
      return;

   sqlite3_finalize(stmt);

   rc = sqlite3_prepare_v2 (
      db, "UPDATE audio_com_pending_blocks SET project_id = ? WHERE project_id = ?", -1, &stmt, nullptr);

   if (rc != SQLITE_OK)
      return;

   sqlite3_bind_int64(stmt, 1, projectId);
   sqlite3_bind_int64(stmt, 2, UNASSIGNED_PROJECT_ID);

   rc = sqlite3_step(stmt);

   if (rc != SQLITE_DONE)
      return;

   sqlite3_finalize(stmt);

   rc = sqlite3_prepare_v2 (
      db, "DELETE FROM audiocom_cloud_sync WHERE project_id != ?", -1, &stmt, nullptr);

   if (rc != SQLITE_OK)
      return;

   sqlite3_bind_int64(stmt, 1, projectId);

   rc = sqlite3_step(stmt);

   if (rc != SQLITE_DONE)
      return;

   transaction.Commit();
}

std::vector<uint8_t> GetProjectData(AudacityProject& project)
{
   auto db = GetProjectDatabaseHandle(project);

   sqlite3_stmt* stmt = nullptr;
   int rc = sqlite3_prepare_v2(
      db, "SELECT dict, doc FROM project WHERE id = 1", -1, &stmt, nullptr);

   if (rc != SQLITE_OK)
      return {};

   auto cleanup = finally([&stmt] { sqlite3_finalize(stmt); });

   rc = sqlite3_step(stmt);

   if (rc != SQLITE_ROW)
      return {};

   auto dictData = sqlite3_column_blob(stmt, 0);
   auto dictDataSize = sqlite3_column_bytes(stmt, 0);

   auto projectData = sqlite3_column_blob(stmt, 1);
   auto projectDataSize = sqlite3_column_bytes(stmt, 1);

   std::vector<uint8_t> result;
   // SQLite uses `int` for size, and it is hard to imagine a project
   // that would be larger than 2GB
   result.resize(dictDataSize + projectDataSize + sizeof(int));

   if (IsLittleEndian())
      std::memcpy(result.data(), &dictDataSize, sizeof(int));
   else
   {
      int swapped = SwapIntBytes(dictDataSize);
      std::memcpy(result.data(), &swapped, sizeof(int));
   }

   std::memcpy(result.data() + sizeof(int), dictData, dictDataSize);
   std::memcpy(result.data() + sizeof(int) + dictDataSize, projectData, projectDataSize);

   return result;
}

} // namespace

CloudProjectSnapshot::CloudProjectSnapshot(
   AudacityProject& project)
    : mProject(project.shared_from_this())
{
   auto [id, version] = GetProjectInfo(project);

   mCloudProjectId = id;
   mCloudVersion = version;

   mMissingBlocks = GetMissingBlocks(project);
}

CloudProjectSnapshot::~CloudProjectSnapshot() = default;

void CloudProjectSnapshot::SaveSnapshot(SnapshotOperationCompleted callback)
{
   mCompletedCallback = std::move(callback);

   mProjectBlocks = GetProjectBlocks(*mProject);
   // Lock all the blocks in the project
   UpdateDBPendingBlocks(mProjectBlocks);

   CreateOrUpdateProject();
}

SampleBlockIDs CloudProjectSnapshot::GetDBPendingBlocks() const
{
   SampleBlockIDs result;

   auto db = GetProjectDatabaseHandle(*mProject);

   sqlite3_stmt* stmt = nullptr;

   int rc = sqlite3_prepare_v2(
      db, "SELECT block_id FROM audio_com_pending_blocks WHERE project_id = ? ORDER BY block_id ASC",
      -1, &stmt, nullptr);

   if (rc != SQLITE_OK)
      return result;

   auto cleanup = finally([&stmt] { sqlite3_finalize(stmt); });

   sqlite3_bind_int64(stmt, 1, mCloudProjectId);

   rc = sqlite3_step(stmt);

   while (rc == SQLITE_ROW)
   {
      result.push_back(sqlite3_column_int64(stmt, 0));
      rc = sqlite3_step(stmt);
   }

   return result;
}

void CloudProjectSnapshot::ModifyPendingBlocks(
   std::string_view query, const SampleBlockIDs& blocks)
{
   auto db = GetProjectDatabaseHandle(*mProject);

   sqlite3_stmt* stmt = nullptr;

   int rc = sqlite3_prepare_v2(db, query.data(), query.size(), &stmt, nullptr);

   if (rc != SQLITE_OK)
      return;

   auto cleanup = finally([&stmt] { sqlite3_finalize(stmt); });

   for (auto block : blocks)
   {
      sqlite3_bind_int64(stmt, 1, mCloudProjectId);
      sqlite3_bind_int64(stmt, 2, block);

      rc = sqlite3_step(stmt);

      if (rc != SQLITE_DONE)
         return;

      sqlite3_reset(stmt);
   }
}

void CloudProjectSnapshot::UpdateDBPendingBlocks(const SampleBlockIDs& blocks)
{
   TransactionScope transaction(*mProject, "CloudSaveUpdatePendingBlocks");

   auto dbBlocks = GetDBPendingBlocks();

   SampleBlockIDs toRemove;
   SampleBlockIDs toAdd;

   for (auto block : blocks)
   {
      // dbBlocks is sorted, see GetDBPendingBlocks
      if (!std::binary_search(dbBlocks.begin(), dbBlocks.end(), block))
         toAdd.push_back(block);
   }

   for (auto block : dbBlocks)
   {
      // Blocks are not sorted, so we have to use find
      if (std::find(blocks.begin(), blocks.end(), block) == blocks.end())
         toRemove.push_back(block);
   }

   ModifyPendingBlocks(DeleteBlocksQuery, toRemove);
   ModifyPendingBlocks(InsertBlocksQuery, toAdd);

   transaction.Commit();
}

void CloudProjectSnapshot::CreateOrUpdateProject()
{
   using namespace audacity::network_manager;

   auto multipartData = std::make_unique<MultipartData>();

   multipartData->Add("project", audacity::ToUTF8(mProject->GetProjectName()));
   multipartData->Add("blocks", SerializeBlockIDs(mProjectBlocks));

   if (mCloudProjectId != UNASSIGNED_PROJECT_ID)
      multipartData->Add("version", std::to_string(mCloudVersion));

   auto data = GetProjectData(*mProject);

   // Data will be copied
   multipartData->Add(
      "file", common_content_types::ApplicationXOctetStream, data.data(),
      data.size());

   const auto url = std::string("http://localhost:8090") +
                    (mCloudProjectId == UNASSIGNED_PROJECT_ID ?
                        "/audacity/project" :
                        "/audacity/project/" + std::to_string(mCloudProjectId));

   auto request = Request(url);

   request.setHeader(
      common_headers::ContentType, common_content_types::MultipartFormData);
   request.setHeader(
      common_headers::Accept, common_content_types::ApplicationJson);
   //request.setHeader(common_headers::ContentEncoding, "gzip");

   // Todo: languages

   auto response =
      NetworkManager::GetInstance().doPost(request, std::move(multipartData));

   response->setRequestFinishedCallback ([this, response](auto response) {
      const auto error = response->getError();

      if (error != NetworkError::NoError)
      {
         auto errorMessage = response->getErrorString();

         if (error == NetworkError::HTTPError)
         {
            errorMessage += "\nHTTP code: " + std::to_string(response->getHTTPCode());
            errorMessage += "\nResponse: " + response->readAll<std::string>();
         }

         mCompletedCallback({ errorMessage, false });
         return;
      }

      auto result =
         DeserializeProjectResponse(response->readAll<std::string>());

      if (!result.has_value())
      {
         mCompletedCallback(
            { audacity::ToUTF8(XO("Invalid Response").Translation()), false });
         return;
      }

      ProjectResponse projectResponse = *result;

      mCloudProjectId = projectResponse.ProjectId;
      mCloudVersion = projectResponse.Version;
      mMissingBlocks = projectResponse.MissingBlocks;

      UpdateProjectInfo(*mProject, mCloudProjectId, mCloudVersion);
      UpdateDBPendingBlocks({});
      AppendMissingBlocks(*mProject, mMissingBlocks);

      if (mMissingBlocks.empty())
         mCompletedCallback({ {}, true });
      else
      {
         mMissingBlockUploader = std::make_unique<MissingBlocksUploader>(
            mProject, mMissingBlocks,
            [this](auto result)
            {
               wxLogDebug(
                  "MissingBlocksUploader: %zu/%zu/%zu", result.UploadedBlocks,
                  result.FailedBlocks, result.TotalBlocks);
            });
      }
   });

}

} // namespace cloud::audiocom::sync
