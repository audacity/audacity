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

#include <algorithm>

namespace cloud::audiocom::sync
{

namespace
{
std::optional<UploadUrls> DeserializeUploadUrls(const rapidjson::Value& value)
{
   if (!value.IsObject())
      return {};

   if (!value.HasMember("id") || !value["id"].IsString())
      return {};

   if (!value.HasMember("url") || !value["url"].IsString())
      return {};

   if (!value.HasMember("success") || !value["success"].IsString())
      return {};

   if (!value.HasMember("fail") || !value["fail"].IsString())
      return {};

   return UploadUrls {
      value["id"].GetString(),
      value["url"].GetString(),
      value["success"].GetString(),
      value["fail"].GetString(),
   };
}

std::optional<UploadUrls> DeserializeUploadUrls(const rapidjson::Value& document, std::string_view key)
{
   if (!document.HasMember(key.data()))
      return {};

   const auto& value = document[key.data()];

   return DeserializeUploadUrls(value);
}

std::optional<ProjectInfo> DeserializeProjectInfo(const rapidjson::Value& document, std::string_view key)
{
   if (!document.HasMember(key.data()))
      return {};

   const auto& value = document[key.data()];

   if (!value.IsObject())
      return {};

   if (!value.HasMember("id") || !value["id"].IsString())
      return {};

   if (!value.HasMember("date_created") || !value["date_created"].IsInt64())
      return {};

   if (!value.HasMember("date_updated") || !value["date_updated"].IsInt64())
      return {};

   return ProjectInfo {
      value["id"].GetString(),
      value["date_created"].GetInt64(),
      value["date_updated"].GetInt64(),
   };
}

std::optional<SnapshotInfo> DeserializeSnapshotInfo (const rapidjson::Value& document, std::string_view key)
{
   if (!document.HasMember(key.data()))
      return {};

   const auto& value = document[key.data()];

   if (!value.IsObject())
      return {};

   if (!value.HasMember("id") || !value["id"].IsString())
      return {};

   return SnapshotInfo{
      value["id"].GetString(),
   };
}

std::optional<std::vector<UploadUrls>> DeserializeBlockUrls (const rapidjson::Value& document, std::string_view key)
{
   if (!document.HasMember(key.data()))
      return {};

   const auto& value = document[key.data()];

   if (!value.IsArray())
      return {};

   std::vector<UploadUrls> result;

   for (const auto& url : value.GetArray ())
   {
      auto uploadUrls = DeserializeUploadUrls(url);

      if (!uploadUrls)
         return {};

      std::transform(
         uploadUrls->Id.begin(), uploadUrls->Id.end(), uploadUrls->Id.begin(),
         [](unsigned char c) { return std::toupper(c); });

      result.push_back(std::move(*uploadUrls));
   }

   return result;
}

std::optional<ProjectSyncState> DeserializeProjectSyncState (const rapidjson::Value& document)
{
   if (!document.IsObject())
      return {};

   auto mixdownUrls = DeserializeUploadUrls(document, "mixdown");
   if (!mixdownUrls)
      return {};

   auto fileUploadUrls = DeserializeUploadUrls(document, "file");
   if (!fileUploadUrls)
      return {};

   auto blockUrls = DeserializeBlockUrls(document, "blocks");
   if (!blockUrls)
      return {};

   return ProjectSyncState{
      std::move(*mixdownUrls),
      std::move(*fileUploadUrls),
      std::move(*blockUrls),
   };
}

std::optional<ProjectSyncState> DeserializeProjectSyncState (
   const rapidjson::Value& document, std::string_view key)
{
   if (!document.HasMember(key.data()))
      return {};

   return DeserializeProjectSyncState(document[key.data()]);
}

} // namespace

std::string SerializeProjectForm(const ProjectForm& form)
{
   using namespace rapidjson;

   Document document;
   document.SetObject();

   if (!form.Name.empty())
      document.AddMember(
         "name", StringRef(form.Name.c_str()), document.GetAllocator());

   if (!form.HeadSnapshotId.empty())
      document.AddMember(
         "head_snapshot_id", StringRef(form.HeadSnapshotId.c_str()),
         document.GetAllocator());

   Value hashesArray(kArrayType);

   for (const auto& hash : form.Hashes)
      hashesArray.PushBack(StringRef(hash.c_str()), document.GetAllocator());

   document.AddMember("blocks", hashesArray, document.GetAllocator());

   StringBuffer buffer;
   Writer<StringBuffer> writer(buffer);
   document.Accept(writer);

   return buffer.GetString();
}

std::optional<ProjectResponse>
DeserializeProjectResponse(const std::string& data)
{
   using namespace rapidjson;

   Document document;
   document.Parse(data.c_str());

   if (!document.IsObject())
      return {};

   auto projectInfo = DeserializeProjectInfo(document, "project");
   if (!projectInfo)
      return {};

   auto snapshotInfo = DeserializeSnapshotInfo(document, "snapshot");
   if (!snapshotInfo)
      return {};

   auto syncState = DeserializeProjectSyncState(document, "sync");
   if (!syncState)
      return {};

   ProjectResponse result;

   result.Project = std::move(*projectInfo);
   result.Snapshot = std::move(*snapshotInfo);
   result.SyncState = std::move(*syncState);

   return result;
}

} // namespace cloud::audiocom::sync
