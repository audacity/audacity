/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSyncDTO.cpp

  Dmitry Vedenko

**********************************************************************/
#include "CloudSyncDTO.h"

#include <numeric>
#include <set>
#include <unordered_set>

#include <rapidjson/document.h>
#include <rapidjson/reader.h>
#include <rapidjson/stringbuffer.h>
#include <rapidjson/writer.h>

#include <algorithm>

#include "wxFileNameWrapper.h"

namespace audacity::cloud::audiocom::sync {
namespace {
bool Deserialize(const rapidjson::Value& value, std::string& result)
{
    if (!value.IsString()) {
        return false;
    }

    result = value.GetString();

    return true;
}

bool Deserialize(const rapidjson::Value& value, int& result)
{
    if (!value.IsInt64()) {
        return false;
    }

    result = value.GetInt();

    return true;
}

bool Deserialize(const rapidjson::Value& value, long& result)
{
    if (!value.IsInt64()) {
        return false;
    }

    if (sizeof(long) == sizeof(int64_t)) {
        result = value.GetInt64();
    } else {
        result = value.GetInt();
    }

    return true;
}

bool Deserialize(const rapidjson::Value& value, long long& result)
{
    if (!value.IsInt64()) {
        return false;
    }

    result = value.GetInt64();

    return true;
}

bool Deserialize(const rapidjson::Value& value, bool& result)
{
    if (!value.IsBool()) {
        return false;
    }

    result = value.GetBool();

    return true;
}

template<typename T>
bool Deserialize(
    const rapidjson::Value& value, std::string_view zKey, T& result);

template<typename T>
bool DeserializeArray(const rapidjson::Value& value, std::vector<T>& result);

template<typename T>
bool DeserializeArray(
    const rapidjson::Value& value, std::string_view zKey, std::vector<T>& result)
{
    if (!value.IsObject() || !value.HasMember(zKey.data())) {
        return false;
    }

    return DeserializeArray(value[zKey.data()], result);
}

bool Deserialize(const rapidjson::Value& value, UploadUrls& urls)
{
    if (!value.IsObject()) {
        return {}
    }

    UploadUrls tempUrls;

    if (!Deserialize(value, "id", tempUrls.Id)) {
        return {}
    }

    if (!Deserialize(value, "url", tempUrls.UploadUrl)) {
        return {}
    }

    if (!Deserialize(value, "success", tempUrls.SuccessUrl)) {
        return {}
    }

    if (!Deserialize(value, "fail", tempUrls.FailUrl)) {
        return {}
    }

    urls = std::move(tempUrls);

    return true;
}

bool Deserialize(const rapidjson::Value& value, VersionInfo& urls)
{
    if (!value.IsObject()) {
        return {}
    }

    VersionInfo tempVersion;

    if (!Deserialize(value, "id", tempVersion.Id)) {
        return {}
    }

    if (!Deserialize(value, "name", tempVersion.Name)) {
        return {}
    }

    if (!Deserialize(value, "snapshot_id", tempVersion.SnapshotId)) {
        return {}
    }

    if (!Deserialize(value, "date_created", tempVersion.Created)) {
        return {}
    }

    if (!Deserialize(value, "date_updated", tempVersion.Updated)) {
        return {}
    }

    urls = std::move(tempVersion);

    return true;
}

bool Deserialize(const rapidjson::Value& value, SnapshotBlockInfo& urls)
{
    if (!value.IsObject()) {
        return {}
    }

    SnapshotBlockInfo tempBlock;

    if (!Deserialize(value, "hash", tempBlock.Hash)) {
        return {}
    }

    if (!Deserialize(value, "url", tempBlock.Url)) {
        return {}
    }

    urls = std::move(tempBlock);

    return true;
}

bool Deserialize(const rapidjson::Value& value, SnapshotInfo& snapshotInfo)
{
    if (!value.IsObject()) {
        return {}
    }

    SnapshotInfo tempSnapshot;

    if (!Deserialize(value, "id", tempSnapshot.Id)) {
        return {}
    }

    if (!Deserialize(value, "parent_id", tempSnapshot.ParentId)) {
        return {}
    }

    if (!Deserialize(value, "date_created", tempSnapshot.Created)) {
        return {}
    }

    if (!Deserialize(value, "date_updated", tempSnapshot.Updated)) {
        return {}
    }

    if (!Deserialize(value, "date_synced", tempSnapshot.Synced)) {
        return {}
    }

    if (!Deserialize(value, "file_size", tempSnapshot.FileSize)) {
        return {}
    }

    if (!Deserialize(value, "blocks_size", tempSnapshot.BlocksSize)) {
        return {}
    }

    Deserialize(value, "file_url", tempSnapshot.FileUrl);
    DeserializeArray(value, "blocks", tempSnapshot.Blocks);

    snapshotInfo = std::move(tempSnapshot);

    return true;
}

bool Deserialize(const rapidjson::Value& value, ProjectInfo& projectInfo)
{
    if (!value.IsObject()) {
        return {}
    }

    ProjectInfo tempProject;

    if (!Deserialize(value, "id", tempProject.Id)) {
        return {}
    }

    if (!Deserialize(value, "username", tempProject.Username)) {
        return {}
    }

    if (!Deserialize(value, "author_name", tempProject.AuthorName)) {
        return {}
    }

    if (!Deserialize(value, "slug", tempProject.Slug)) {
        return {}
    }

    if (!Deserialize(value, "name", tempProject.Name)) {
        return {}
    }

    if (!Deserialize(value, "details", tempProject.Details)) {
        return {}
    }

    if (!Deserialize(value, "date_created", tempProject.Created)) {
        return {}
    }

    if (!Deserialize(value, "date_updated", tempProject.Updated)) {
        return {}
    }

    Deserialize(value, "head", tempProject.HeadSnapshot);
    Deserialize(
        value, "latest_synced_snapshot_id", tempProject.LastSyncedSnapshotId);

    projectInfo = std::move(tempProject);

    return true;
}

bool Deserialize(const rapidjson::Value& value, ProjectSyncState& urls)
{
    if (!value.IsObject()) {
        return {}
    }

    ProjectSyncState tempState;

    if (!Deserialize(value, "mixdown", tempState.MixdownUrls)) {
        return {}
    }

    if (!Deserialize(value, "file", tempState.FileUrls)) {
        return {}
    }

    if (!DeserializeArray(value, "blocks", tempState.MissingBlocks)) {
        return {}
    }

    urls = std::move(tempState);

    return true;
}

bool Deserialize(const rapidjson::Value& value, CreateSnapshotResponse& reponse)
{
    if (!value.IsObject()) {
        return {}
    }

    CreateSnapshotResponse tempResponse;

    if (!Deserialize(value, "project", tempResponse.Project)) {
        return {}
    }

    if (!Deserialize(value, "snapshot", tempResponse.Snapshot)) {
        return {}
    }

    if (!Deserialize(value, "sync", tempResponse.SyncState)) {
        return {}
    }

    reponse = std::move(tempResponse);

    return true;
}

bool Deserialize(const rapidjson::Value& value, PaginationInfo& info)
{
    if (!value.IsObject()) {
        return {}
    }

    PaginationInfo tempInfo;

    if (!Deserialize(value, "total", tempInfo.TotalCount)) {
        return {}
    }

    if (!Deserialize(value, "pages", tempInfo.PagesCount)) {
        return {}
    }

    if (!Deserialize(value, "page", tempInfo.CurrentPage)) {
        return {}
    }

    if (!Deserialize(value, "size", tempInfo.PageSize)) {
        return {}
    }

    info = std::move(tempInfo);

    return true;
}

bool Deserialize(
    const rapidjson::Value& value, PaginatedProjectsResponse& response)
{
    if (!value.IsObject()) {
        return {}
    }

    PaginatedProjectsResponse tempResponse;

    if (!DeserializeArray(value, "items", tempResponse.Items)) {
        return {}
    }

    if (!Deserialize(value, "pagination", tempResponse.Pagination)) {
        return {}
    }

    response = std::move(tempResponse);

    return true;
}

template<typename T>
bool Deserialize(
    const rapidjson::Value& value, std::string_view zKey, T& result)
{
    if (!value.IsObject() || !value.HasMember(zKey.data())) {
        return false;
    }

    return Deserialize(value[zKey.data()], result);
}

template<typename T>
bool DeserializeArray(const rapidjson::Value& value, std::vector<T>& result)
{
    if (!value.IsArray()) {
        return false;
    }

    result.clear();
    result.reserve(value.Size());

    for (const auto& item : value.GetArray()) {
        T value;

        if (!Deserialize(item, value)) {
            return false;
        }

        result.push_back(std::move(value));
    }

    return true;
}

template<typename T>
bool Deserialize(std::string_view data, T& result)
{
    rapidjson::Document document;
    document.Parse(data.data(), data.size());

    if (document.HasParseError()) {
        return false;
    }

    return Deserialize(document, result);
}
} // namespace

std::string Serialize(const ProjectForm& form)
{
    using namespace rapidjson;

    Document document;
    document.SetObject();

    if (!form.Name.empty()) {
        document.AddMember(
            "name", StringRef(form.Name.c_str()), document.GetAllocator());
    }

    if (!form.HeadSnapshotId.empty()) {
        document.AddMember(
            "head_snapshot_id", StringRef(form.HeadSnapshotId.c_str()),
            document.GetAllocator());
    }

    Value hashesArray(kArrayType);

    for (const auto& hash : form.Hashes) {
        hashesArray.PushBack(StringRef(hash.c_str()), document.GetAllocator());
    }

    document.AddMember("blocks", hashesArray, document.GetAllocator());

    if (form.Force) {
        document.AddMember("force", form.Force, document.GetAllocator());
    }

    StringBuffer buffer;
    Writer<StringBuffer> writer(buffer);
    document.Accept(writer);

    return buffer.GetString();
}

std::optional<ProjectSyncState>
DeserializeProjectSyncState(const std::string& data)
{
    ProjectSyncState result;

    if (Deserialize(data, result)) {
        return std::move(result);
    }

    return {};
}

std::optional<CreateSnapshotResponse>
DeserializeCreateSnapshotResponse(const std::string& data)
{
    CreateSnapshotResponse result;

    if (Deserialize(data, result)) {
        return std::move(result);
    }

    return {};
}

std::optional<PaginatedProjectsResponse>
DeserializePaginatedProjectsResponse(const std::string& data)
{
    PaginatedProjectsResponse result;

    if (Deserialize(data, result)) {
        return std::move(result);
    }

    return {};
}

std::optional<ProjectInfo> DeserializeProjectInfo(const std::string& data)
{
    ProjectInfo result;

    if (Deserialize(data, result)) {
        return std::move(result);
    }

    return {};
}

std::optional<SnapshotInfo> DeserializeSnapshotInfo(const std::string& data)
{
    SnapshotInfo result;

    if (Deserialize(data, result)) {
        return std::move(result);
    }

    return {};
}

namespace {
wxString SafeName(wxString name)
{
    static const std::unordered_set<wxChar> invalidChars {
        L'/', L'\\', L':', L'*', L'?', L'"', L'\'', L'<', L'>', L'|', L'@',
        L'&', '$',   L'%', L'^', L';', L'~', L'`',  L'[', L']', L'{', L'}',
    };

    for (size_t i = 0; i < name.length(); ++i) {
        if (invalidChars.find(name[i]) != invalidChars.end()) {
            name[i] = wxT('_');
        }
    }

    return name;
}
} // namespace

wxString
MakeSafeProjectPath(const wxString& rootDir, const wxString& projectName)
{
    const auto safeName = SafeName(projectName);

    auto path = wxFileNameWrapper { rootDir, safeName, "aup3" };

    int iteration = 0;
    while (path.FileExists()) {
        path.SetName(wxString::Format("%s_%d", safeName, ++iteration));
    }

    return path.GetFullPath();
}

std::string Serialize(NetworkStats stats)
{
    using namespace rapidjson;

    Document document;
    document.SetObject();

    document.AddMember("is_download", stats.IsDownload, document.GetAllocator());
    document.AddMember("bytes", stats.Bytes, document.GetAllocator());
    document.AddMember("blocks", stats.Blocks, document.GetAllocator());
    document.AddMember("mixes", stats.Mixes, document.GetAllocator());
    document.AddMember("files", stats.Files, document.GetAllocator());

    StringBuffer buffer;
    Writer<StringBuffer> writer(buffer);
    document.Accept(writer);

    return buffer.GetString();
}
} // namespace audacity::cloud::audiocom::sync
