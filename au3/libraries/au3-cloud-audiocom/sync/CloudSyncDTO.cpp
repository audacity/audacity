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

#include "au3-import-export/rapidjson/document.h"
#include "au3-import-export/rapidjson/reader.h"
#include "au3-import-export/rapidjson/stringbuffer.h"
#include "au3-import-export/rapidjson/writer.h"

#include <algorithm>

#include "au3-files/wxFileNameWrapper.h"

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
        return {};
    }

    UploadUrls tempUrls;

    if (!Deserialize(value, "id", tempUrls.Id)) {
        return {};
    }

    if (!Deserialize(value, "url", tempUrls.UploadUrl)) {
        return {};
    }

    if (!Deserialize(value, "success", tempUrls.SuccessUrl)) {
        return {};
    }

    if (!Deserialize(value, "fail", tempUrls.FailUrl)) {
        return {};
    }

    urls = std::move(tempUrls);

    return true;
}

bool Deserialize(const rapidjson::Value& value, VersionInfo& urls)
{
    if (!value.IsObject()) {
        return {};
    }

    VersionInfo tempVersion;

    if (!Deserialize(value, "id", tempVersion.Id)) {
        return {};
    }

    if (!Deserialize(value, "name", tempVersion.Name)) {
        return {};
    }

    if (!Deserialize(value, "snapshot_id", tempVersion.SnapshotId)) {
        return {};
    }

    if (!Deserialize(value, "date_created", tempVersion.Created)) {
        return {};
    }

    if (!Deserialize(value, "date_updated", tempVersion.Updated)) {
        return {};
    }

    urls = std::move(tempVersion);

    return true;
}

bool Deserialize(const rapidjson::Value& value, SnapshotBlockInfo& urls)
{
    if (!value.IsObject()) {
        return {};
    }

    SnapshotBlockInfo tempBlock;

    if (!Deserialize(value, "hash", tempBlock.Hash)) {
        return {};
    }

    if (!Deserialize(value, "url", tempBlock.Url)) {
        return {};
    }

    urls = std::move(tempBlock);

    return true;
}

bool Deserialize(const rapidjson::Value& value, SnapshotInfo& snapshotInfo)
{
    if (!value.IsObject()) {
        return {};
    }

    SnapshotInfo tempSnapshot;

    if (!Deserialize(value, "id", tempSnapshot.Id)) {
        return {};
    }

    if (!Deserialize(value, "parent_id", tempSnapshot.ParentId)) {
        return {};
    }

    if (!Deserialize(value, "date_created", tempSnapshot.Created)) {
        return {};
    }

    if (!Deserialize(value, "date_updated", tempSnapshot.Updated)) {
        return {};
    }

    if (!Deserialize(value, "date_synced", tempSnapshot.Synced)) {
        return {};
    }

    if (!Deserialize(value, "file_size", tempSnapshot.FileSize)) {
        return {};
    }

    if (!Deserialize(value, "blocks_size", tempSnapshot.BlocksSize)) {
        return {};
    }

    Deserialize(value, "file_url", tempSnapshot.FileUrl);
    DeserializeArray(value, "blocks", tempSnapshot.Blocks);

    snapshotInfo = std::move(tempSnapshot);

    return true;
}

bool Deserialize(const rapidjson::Value& value, ProjectInfo& projectInfo)
{
    if (!value.IsObject()) {
        return {};
    }

    ProjectInfo tempProject;

    if (!Deserialize(value, "id", tempProject.Id)) {
        return {};
    }

    if (!Deserialize(value, "username", tempProject.Username)) {
        return {};
    }

    if (!Deserialize(value, "author_name", tempProject.AuthorName)) {
        return {};
    }

    if (!Deserialize(value, "slug", tempProject.Slug)) {
        return {};
    }

    if (!Deserialize(value, "name", tempProject.Name)) {
        return {};
    }

    if (!Deserialize(value, "details", tempProject.Details)) {
        return {};
    }

    if (!Deserialize(value, "date_created", tempProject.Created)) {
        return {};
    }

    if (!Deserialize(value, "date_updated", tempProject.Updated)) {
        return {};
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
        return {};
    }

    ProjectSyncState tempState;

    if (!Deserialize(value, "mixdown", tempState.MixdownUrls)) {
        return {};
    }

    if (!Deserialize(value, "file", tempState.FileUrls)) {
        return {};
    }

    if (!DeserializeArray(value, "blocks", tempState.MissingBlocks)) {
        return {};
    }

    urls = std::move(tempState);

    return true;
}

bool Deserialize(const rapidjson::Value& value, CreateSnapshotResponse& reponse)
{
    if (!value.IsObject()) {
        return {};
    }

    CreateSnapshotResponse tempResponse;

    if (!Deserialize(value, "project", tempResponse.Project)) {
        return {};
    }

    if (!Deserialize(value, "snapshot", tempResponse.Snapshot)) {
        return {};
    }

    if (!Deserialize(value, "sync", tempResponse.SyncState)) {
        return {};
    }

    reponse = std::move(tempResponse);

    return true;
}

bool Deserialize(const rapidjson::Value& value, PaginationInfo& info)
{
    if (!value.IsObject()) {
        return {};
    }

    PaginationInfo tempInfo;

    if (!Deserialize(value, "total", tempInfo.TotalCount)) {
        return {};
    }

    if (!Deserialize(value, "pages", tempInfo.PagesCount)) {
        return {};
    }

    if (!Deserialize(value, "page", tempInfo.CurrentPage)) {
        return {};
    }

    if (!Deserialize(value, "size", tempInfo.PageSize)) {
        return {};
    }

    info = std::move(tempInfo);

    return true;
}

bool Deserialize(
    const rapidjson::Value& value, PaginatedProjectsResponse& response)
{
    if (!value.IsObject()) {
        return {};
    }

    PaginatedProjectsResponse tempResponse;

    if (!DeserializeArray(value, "items", tempResponse.Items)) {
        return {};
    }

    if (!Deserialize(value, "pagination", tempResponse.Pagination)) {
        return {};
    }

    response = std::move(tempResponse);

    return true;
}

bool Deserialize(const rapidjson::Value& value, CloudAudioDownloadInfoItem& item)
{
    if (!value.IsObject()) {
        return false;
    }

    CloudAudioDownloadInfoItem temp;

    if (!Deserialize(value, "format", temp.Format)) {
        return false;
    }

    if (!Deserialize(value, "url", temp.Url)) {
        return false;
    }

    if (!Deserialize(value, "size", temp.Size)) {
        return false;
    }

    if (!Deserialize(value, "source", temp.IsSource)) {
        return false;
    }

    item = std::move(temp);
    return true;
}

bool Deserialize(const rapidjson::Value& value, CloudAudioDownloadInfo& info)
{
    CloudAudioDownloadInfo temp;

    if (value.IsArray()) {
        if (!DeserializeArray(value, temp.Items)) {
            return false;
        }
    } else if (value.IsObject() && value.HasMember("items")) {
        if (!DeserializeArray(value, "items", temp.Items)) {
            return false;
        }
    } else {
        return false;
    }

    info = std::move(temp);
    return true;
}

bool Deserialize(const rapidjson::Value& value, CloudAudioInfo& audio)
{
    if (!value.IsObject()) {
        return {};
    }

    CloudAudioInfo temp;

    if (!Deserialize(value, "id", temp.Id)) {
        return {};
    }

    if (!Deserialize(value, "username", temp.Username)) {
        return {};
    }

    if (!Deserialize(value, "author", temp.AuthorName)) {
        return {};
    }

    if (!Deserialize(value, "slug", temp.Slug)) {
        return {};
    }

    if (!Deserialize(value, "title", temp.Title)) {
        return {};
    }

    if (value.HasMember("tags") && value["tags"].IsArray()) {
        DeserializeArray(value["tags"], temp.Tags);
    }

    if (!Deserialize(value, "created", temp.Created)) {
        return {};
    }

    audio = std::move(temp);
    return true;
}

bool Deserialize(const rapidjson::Value& value, CloudAudioFullInfo& audio)
{
    if (!value.IsObject()) {
        return {};
    }

    CloudAudioFullInfo temp;

    if (!Deserialize(value, "id", temp.Id)) {
        return {};
    }

    if (!Deserialize(value, "username", temp.Username)) {
        return {};
    }

    if (!Deserialize(value, "author_name", temp.AuthorName)) {
        return {};
    }

    if (!Deserialize(value, "slug", temp.Slug)) {
        return {};
    }

    if (!Deserialize(value, "title", temp.Title)) {
        return {};
    }

    if (value.HasMember("tags") && value["tags"].IsArray()) {
        DeserializeArray(value["tags"], temp.Tags);
    }

    if (!Deserialize(value, "date_created", temp.Created)) {
        return {};
    }

    if (!Deserialize(value, "is_downloadable", temp.IsDownloadable)) {
        return {};
    }

    if (value.HasMember("author") && value["author"].IsObject()) {
        if (!Deserialize(value["author"], "id", temp.AuthorId)) {
            return {};
        }
    }

    audio = std::move(temp);
    return true;
}

bool Deserialize(
    const rapidjson::Value& value, PaginatedAudioResponse& response)
{
    if (!value.IsObject()) {
        return {};
    }

    PaginatedAudioResponse tempResponse;

    if (!DeserializeArray(value, "items", tempResponse.Items)) {
        return {};
    }

    if (!Deserialize(value, "pagination", tempResponse.Pagination)) {
        return {};
    }

    response = std::move(tempResponse);

    return true;
}

bool Deserialize(const rapidjson::Value& value, PollingInfo& task)
{
    if (!value.IsObject()) {
        return {};
    }

    PollingInfo tempPolling;

    if (!Deserialize(value, "interval", tempPolling.IntervalSeconds)) {
        return {};
    }

    if (!Deserialize(value, "stop", tempPolling.Stop)) {
        return {};
    }

    task = std::move(tempPolling);

    return true;
}

bool Deserialize(const rapidjson::Value& value, AudioOpenTaskParameters& task)
{
    if (!value.IsObject()) {
        return {};
    }

    AudioOpenTaskParameters tempTask;

    if (!Deserialize(value, "audio_id", tempTask.AudioId)) {
        return {};
    }

    task = std::move(tempTask);

    return true;
}

bool Deserialize(const rapidjson::Value& value, ProjectOpenTaskParameters& task)
{
    if (!value.IsObject()) {
        return {};
    }

    ProjectOpenTaskParameters tempTask;

    if (!Deserialize(value, "project_id", tempTask.ProjectId)) {
        return {};
    }

    // Snapshot ID is optional
    Deserialize(value, "snapshot_id", tempTask.SnapshotId);

    task = std::move(tempTask);

    return true;
}

bool Deserialize(const rapidjson::Value& value, TaskStatus& taskStatus)
{
    if (!value.IsString()) {
        return false;
    }

    const std::string typeStr = value.GetString();

    if (typeStr == "pending") {
        taskStatus = TaskStatus::Pending;
    } else if (typeStr == "started") {
        taskStatus = TaskStatus::Started;
    } else if (typeStr == "success") {
        taskStatus = TaskStatus::Success;
    } else if (typeStr == "failed") {
        taskStatus = TaskStatus::Failed;
    } else {
        taskStatus = TaskStatus::Unknown;
    }

    return true;
}

bool Deserialize(const rapidjson::Value& value, TaskAction& taskAction)
{
    if (!value.IsString()) {
        return false;
    }

    const std::string typeStr = value.GetString();

    if (typeStr == "open_audio") {
        taskAction = TaskAction::OpenAudio;
    } else if (typeStr == "open_project") {
        taskAction = TaskAction::OpenProject;
    } else {
        taskAction = TaskAction::Unknown;
    }

    return true;
}

bool Deserialize(const rapidjson::Value& value, AppTask& task)
{
    if (!value.IsObject()) {
        return false;
    }

    AppTask tempTask;

    if (!Deserialize(value, "id", tempTask.Id)) {
        return false;
    }

    if (!Deserialize(value, "status", tempTask.Status)) {
        return false;
    }

    if (!Deserialize(value, "action", tempTask.Action)) {
        return false;
    }

    if (!Deserialize(value, "created_at", tempTask.Created)) {
        return false;
    }

    if (ProjectOpenTaskParameters parameters; Deserialize(value, "parameters", parameters)) {
        tempTask.Parameters = std::move(parameters);
    } else if (AudioOpenTaskParameters parameters; Deserialize(value, "parameters", parameters)) {
        tempTask.Parameters = std::move(parameters);
    }

    task = std::move(tempTask);

    return true;
}

bool Deserialize(const rapidjson::Value& value, AppTaskPollResponse& response)
{
    if (!value.IsObject()) {
        return false;
    }

    AppTaskPollResponse tempResponse;

    if (!Deserialize(value, "polling", tempResponse.Polling)) {
        return false;
    }

    if (!DeserializeArray(value, "tasks", tempResponse.Tasks)) {
        return false;
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

std::optional<PaginatedAudioResponse>
DeserializePaginatedAudioResponse(const std::string& data)
{
    PaginatedAudioResponse result;

    if (Deserialize(data, result)) {
        return std::move(result);
    }

    return {};
}

std::optional<CloudAudioInfo> DeserializeCloudAudioInfo(const std::string& data)
{
    CloudAudioInfo result;

    if (Deserialize(data, result)) {
        return std::move(result);
    }

    return {};
}

std::optional<CloudAudioFullInfo> DeserializeCloudAudioFullInfo(const std::string& data)
{
    CloudAudioFullInfo result;

    if (Deserialize(data, result)) {
        return std::move(result);
    }

    return {};
}

std::optional<CloudAudioDownloadInfo>
DeserializeCloudAudioDownloadInfo(const std::string& data)
{
    CloudAudioDownloadInfo result;

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

std::optional<AppTaskPollResponse> DeserializeAppTaskPollResponse(const std::string& data)
{
    AppTaskPollResponse result;

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
MakeSafeFilePath(const wxString& rootDir, const wxString& fileName, const wxString& fileExtension)
{
    const auto safeName = SafeName(fileName);

    auto path = wxFileNameWrapper { rootDir, safeName, fileExtension };

    int iteration = 0;
    while (path.FileExists()) {
        path.SetName(wxString::Format("%s_%d", safeName, ++iteration));
    }

    return path.GetFullPath();
}

wxString
MakeSafeProjectPath(const wxString& rootDir, const wxString& projectName)
{
    return MakeSafeFilePath(rootDir, projectName, "aup3");
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
