/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSyncService.cpp

  Dmitry Vedenko

**********************************************************************/
#include "CloudSyncService.h"

#include <algorithm>
#include <cassert>
#include <chrono>
#include <mutex>
#include <string>

#include "CloudLibrarySettings.h"

#include "sync/CloudProjectsDatabase.h"
#include "sync/CloudSyncDTO.h"
#include "sync/RemoteProjectSnapshot.h"

#include "CodeConversions.h"

#include "OAuthService.h"
#include "ServiceConfig.h"

#include "MemoryX.h"

#include "Project.h"
#include "ProjectFileIO.h"

#include "BasicUI.h"
#include "FileNames.h"

#include "IResponse.h"
#include "NetworkManager.h"
#include "Request.h"

namespace audacity::cloud::audiocom {
namespace {
std::mutex& GetResponsesMutex()
{
    static std::mutex mutex;
    return mutex;
}

std::vector<std::shared_ptr<audacity::network_manager::IResponse> >&
GetPendingRequests()
{
    static std::vector<std::shared_ptr<audacity::network_manager::IResponse> >
    requests;

    return requests;
}

void RemovePendingRequest(audacity::network_manager::IResponse* request)
{
    std::lock_guard lock { GetResponsesMutex() };

    auto& requests = GetPendingRequests();

    requests.erase(
        std::remove_if(
            requests.begin(), requests.end(),
            [request](const auto& r) { return r.get() == request; }),
        requests.end());
}

void PerformProjectGetRequest(
    OAuthService& oAuthService, std::string url,
    std::function<void(ResponseResult)> dataCallback)
{
    assert(oAuthService.HasAccessToken());

    if (!oAuthService.HasAccessToken()) {
        dataCallback({ SyncResultCode::Unauthorized });
        return;
    }

    using namespace audacity::network_manager;

    auto request = Request(std::move(url));

    request.setHeader(
        common_headers::ContentType, common_content_types::ApplicationJson);

    request.setHeader(
        common_headers::Accept, common_content_types::ApplicationJson);

    SetCommonHeaders(request);

    auto response = NetworkManager::GetInstance().doGet(request);

    response->setRequestFinishedCallback(
        [dataCallback = std::move(dataCallback)](auto response)
    {
        BasicUI::CallAfter(
            [dataCallback = std::move(dataCallback), response]
        {
            auto removeRequest
                =finally([response] { RemovePendingRequest(response); });

            dataCallback(GetResponseResult(*response, true));
        });
    });

    std::lock_guard lock { GetResponsesMutex() };
    GetPendingRequests().emplace_back(std::move(response));
}

void GetProjectInfo(
    OAuthService& oAuthService, const ServiceConfig& serviceConfig,
    std::string projectId,
    std::function<void(sync::ProjectInfo, ResponseResult)> callback)
{
    assert(callback);

    PerformProjectGetRequest(
        oAuthService, serviceConfig.GetProjectInfoUrl(projectId),
        [callback = std::move(callback)](ResponseResult result)
    {
        if (result.Code != SyncResultCode::Success) {
            callback(sync::ProjectInfo {}, std::move(result));
            return;
        }

        auto projectInfo = sync::DeserializeProjectInfo(result.Content);

        if (!projectInfo) {
            callback(
                {}, { SyncResultCode::UnexpectedResponse,
                      std::move(result.Content) });
            return;
        }

        callback(std::move(*projectInfo), std::move(result));
    });
}

void GetSnapshotInfo(
    OAuthService& oAuthService, const ServiceConfig& serviceConfig,
    std::string projectId, std::string snapshotId,
    std::function<void(sync::SnapshotInfo, ResponseResult result)> callback)
{
    assert(callback);

    PerformProjectGetRequest(
        oAuthService, serviceConfig.GetSnapshotInfoUrl(projectId, snapshotId),
        [callback = std::move(callback)](ResponseResult result)
    {
        if (result.Code != SyncResultCode::Success) {
            callback({}, std::move(result));
            return;
        }

        auto snapshotInfo = sync::DeserializeSnapshotInfo(result.Content);

        if (!snapshotInfo) {
            callback(
                {}, { SyncResultCode::UnexpectedResponse,
                      std::move(result.Content) });
            return;
        }

        callback(std::move(*snapshotInfo), std::move(result));
    });
}

void GetSnapshotInfo(
    OAuthService& oAuthService, const ServiceConfig& serviceConfig,
    std::string projectId, std::string snapshotId,
    std::function<
        void(sync::ProjectInfo, sync::SnapshotInfo, ResponseResult result)>
    callback)
{
    assert(callback);

    GetProjectInfo(
        oAuthService, serviceConfig, projectId,
        [callback   = std::move(callback), projectId,
         snapshotId = std::move(snapshotId), &oAuthService,
         &serviceConfig](sync::ProjectInfo projectInfo, ResponseResult result)
    {
        if (result.Code != SyncResultCode::Success) {
            callback({}, {}, std::move(result));
            return;
        }

        const auto id = !snapshotId.empty()
                        ? snapshotId
                        : (projectInfo.HeadSnapshot.Synced > 0
                           ? projectInfo.HeadSnapshot.Id
                           : projectInfo.LastSyncedSnapshotId);

        if (id.empty()) {
            callback(
                std::move(projectInfo), sync::SnapshotInfo {},
                { SyncResultCode::Success });
            return;
        }

        GetSnapshotInfo(
            oAuthService, serviceConfig, projectId, id,
            [callback    = std::move(callback),
             projectInfo = std::move(projectInfo)](
                sync::SnapshotInfo snapshotInfo, ResponseResult result)
        {
            callback(
                std::move(projectInfo), std::move(snapshotInfo),
                std::move(result));
        });
    });
}

bool HasAutosave(const std::string& path)
{
    auto connection = sqlite::Connection::Open(path, sqlite::OpenMode::ReadOnly);

    if (!connection) {
        return false;
    }

    if (!connection->CheckTableExists("autosave")) {
        return false;
    }

    auto statement
        =connection->CreateStatement("SELECT COUNT(1) FROM autosave");

    if (!statement) {
        return false;
    }

    auto result = statement->Prepare().Run();

    if (!result.IsOk()) {
        return false;
    }

    for (const auto& row : result) {
        if (row.GetOr(0, 0) > 0) {
            return true;
        }
    }

    return false;
}

bool DropAutosave(const std::string& path)
{
    auto connection
        =sqlite::Connection::Open(path, sqlite::OpenMode::ReadWrite);

    if (!connection) {
        return false;
    }

    if (!connection->CheckTableExists("autosave")) {
        return false;
    }

    auto statement = connection->CreateStatement("DELETE FROM autosave");

    if (!statement) {
        return false;
    }

    auto result = statement->Prepare().Run();

    return result.IsOk();
}
} // namespace

CloudSyncService& CloudSyncService::Get()
{
    static CloudSyncService service;
    return service;
}

CloudSyncService::GetProjectsFuture CloudSyncService::GetProjects(
    concurrency::CancellationContextPtr context, int page, int pageSize,
    std::string_view searchString)
{
    using namespace audacity::network_manager;

    auto promise = std::make_shared<GetProjectsPromise>();

    auto& serviceConfig = GetServiceConfig();
    auto& oAuthService  = GetOAuthService();

    auto request
        =Request(serviceConfig.GetProjectsUrl(page, pageSize, searchString));

    request.setHeader(
        common_headers::ContentType, common_content_types::ApplicationJson);
    request.setHeader(
        common_headers::Accept, common_content_types::ApplicationJson);

    SetCommonHeaders(request);

    auto response = NetworkManager::GetInstance().doGet(request);

    context->OnCancelled(response);

    response->setRequestFinishedCallback(
        [promise, response](auto)
    {
        auto responseResult = GetResponseResult(*response, true);

        if (responseResult.Code != SyncResultCode::Success) {
            promise->set_value(responseResult);
            return;
        }

        auto projects
            =sync::DeserializePaginatedProjectsResponse(responseResult.Content);

        if (!projects) {
            promise->set_value(
                ResponseResult { SyncResultCode::UnexpectedResponse,
                                 std::move(responseResult.Content) });
            return;
        }

        promise->set_value(std::move(*projects));
    });

    return promise->get_future();
}

CloudSyncService::SyncFuture CloudSyncService::OpenFromCloud(
    std::string projectId, std::string snapshotId, SyncMode mode,
    sync::ProgressCallback callback)
{
    ASSERT_MAIN_THREAD();

    // Reset promise
    mSyncPromise = {};

    if (mSyncInProcess.exchange(true)) {
        CompleteSync({ sync::ProjectSyncResult::StatusCode::Blocked, {}, {} });
        return mSyncPromise.get_future();
    }

    if (projectId.empty()) {
        FailSync({ SyncResultCode::InternalClientError, "Empty projectId" });
        return mSyncPromise.get_future();
    }

    if (!callback) {
        mProgressCallback = [](auto...) { return true; } } else {
        mProgressCallback = std::move(callback);
    }

    GetSnapshotInfo(
        GetOAuthService(), GetServiceConfig(), projectId, snapshotId,
        [this, mode](
            sync::ProjectInfo projectInfo, sync::SnapshotInfo snapshotInfo,
            ResponseResult result)
    {
        if (result.Code != SyncResultCode::Success) {
            FailSync(std::move(result));
        } else {
            SyncCloudSnapshot(projectInfo, snapshotInfo, mode);
        }
    });

    return mSyncPromise.get_future();
}

CloudSyncService::SyncFuture CloudSyncService::SyncProject(
    AudacityProject& project, const std::string& path, bool forceSync,
    sync::ProgressCallback callback)
{
    ASSERT_MAIN_THREAD();

    // Reset promise
    mSyncPromise = {};

    if (mSyncInProcess.exchange(true)) {
        CompleteSync({ sync::ProjectSyncResult::StatusCode::Blocked });
        return mSyncPromise.get_future();
    }

    if (!callback) {
        mProgressCallback = [](auto...) { return true; } } else {
        mProgressCallback = std::move(callback);
    }

    auto& cloudDatabase = sync::CloudProjectsDatabase::Get();

    auto projectInfo = cloudDatabase.GetProjectDataForPath(path);

    if (!projectInfo) {
        // We assume, that the project is local
        CompleteSync(path);
        return mSyncPromise.get_future();
    }

    GetProjectInfo(
        GetOAuthService(), GetServiceConfig(), projectInfo->ProjectId,
        [projectInfo = *projectInfo, &project, path,
         mode        = forceSync ? SyncMode::ForceOverwrite : SyncMode::Normal,
         this](sync::ProjectInfo remoteInfo, ResponseResult result)
    {
        if (result.Code != SyncResultCode::Success) {
            FailSync(std::move(result));
            return;
        }

        // Do not perform the snapshot request if the project is up to date
        if (remoteInfo.HeadSnapshot.Id == projectInfo.SnapshotId) {
            CompleteSync({ sync::ProjectSyncResult::StatusCode::Succeeded });
            return;
        }

        const auto snapshotId = remoteInfo.HeadSnapshot.Synced > 0
                                ? remoteInfo.HeadSnapshot.Id
                                : remoteInfo.LastSyncedSnapshotId;

        GetSnapshotInfo(
            GetOAuthService(), GetServiceConfig(), remoteInfo.Id, snapshotId,
            [this, &project, mode,
             remoteInfo](sync::SnapshotInfo snapshotInfo, ResponseResult result)
        {
            if (result.Code != SyncResultCode::Success) {
                FailSync(std::move(result));
            } else {
                SyncCloudSnapshot(remoteInfo, snapshotInfo, mode);
            }
        });
    });

    return mSyncPromise.get_future();
}

bool CloudSyncService::IsCloudProject(const std::string& path)
{
    auto& cloudDatabase = sync::CloudProjectsDatabase::Get();
    auto projectInfo    = cloudDatabase.GetProjectDataForPath(path);

    return projectInfo.has_value();
}

CloudSyncService::ProjectState
CloudSyncService::GetProjectState(const std::string& projectId)
{
    auto& cloudDatabase = sync::CloudProjectsDatabase::Get();

    auto projectInfo = cloudDatabase.GetProjectData(projectId);

    if (!projectInfo) {
        return ProjectState::NotAvaliable;
    }

    if (!wxFileExists(ToWXString(projectInfo->LocalPath))) {
        return ProjectState::NotAvaliable;
    }

    if (projectInfo->SyncStatus == sync::DBProjectData::SyncStatusSynced) {
        return ProjectState::FullySynced;
    }

    return ProjectState::PendingSync;
}

CloudSyncService::GetHeadSnapshotIDFuture
CloudSyncService::GetHeadSnapshotID(std::string_view projectId)
{
    ASSERT_MAIN_THREAD();

    auto promise = std::make_shared<GetHeadSnapshotIDPromise>();

    GetProjectInfo(
        GetOAuthService(), GetServiceConfig(), std::string(projectId),
        [promise](sync::ProjectInfo projectInfo, ResponseResult result)
    {
        if (result.Code != SyncResultCode::Success) {
            promise->set_value(
                sync::GetHeadSnapshotIDResult { std::move(result) });
        } else {
            promise->set_value({ projectInfo.HeadSnapshot.Id });
        }
    });

    return promise->get_future();
}

void CloudSyncService::FailSync(ResponseResult responseResult)
{
    CompleteSync(
        sync::ProjectSyncResult { sync::ProjectSyncResult::StatusCode::Failed,
                                  std::move(responseResult),
                                  {} });
}

void CloudSyncService::CompleteSync(std::string path)
{
    CompleteSync(sync::ProjectSyncResult {
            sync::ProjectSyncResult::StatusCode::Succeeded, {}, std::move(path) });
}

void CloudSyncService::CompleteSync(sync::ProjectSyncResult result)
{
    if (mRemoteSnapshot) {
        result.Stats = mRemoteSnapshot->GetTransferStats();
        ReportUploadStats(mRemoteSnapshot->GetProjectId(), result.Stats);
    }

    mSyncPromise.set_value(std::move(result));
    mRemoteSnapshot.reset();
    mSyncInProcess.store(false);
}

void CloudSyncService::SyncCloudSnapshot(
    const sync::ProjectInfo& projectInfo, const sync::SnapshotInfo& snapshotInfo,
    SyncMode mode)
{
    // Get the project location
    auto localProjectInfo
        =sync::CloudProjectsDatabase::Get().GetProjectData(projectInfo.Id);

    const auto createNew = !localProjectInfo || mode == SyncMode::ForceNew;

    const auto wxPath = createNew
                        ? sync::MakeSafeProjectPath(
        CloudProjectsSavePath.Read(),
        audacity::ToWXString(projectInfo.Name))
                        : audacity::ToWXString(localProjectInfo->LocalPath);

    const auto utf8Path = audacity::ToUTF8(wxPath);

    const auto fileExists = wxFileExists(wxPath);

    if (!fileExists) {
        if (snapshotInfo.Id.empty()) {
            FailSync({ SyncResultCode::SyncImpossible });
            return;
        }

        const auto dir = CloudProjectsSavePath.Read();
        FileNames::MkDir(dir);

        InvisibleTemporaryProject project;
        ProjectFileIO::Get(project.Project()).LoadProject(wxPath, true);
    } else {
        assert(localProjectInfo.has_value());
        assert(mode != SyncMode::ForceNew);
        // The project exists on the disk. Depending on how we got here, we might
        // different scenarios:
        // 1. Local snapshot ID matches the remote snapshot ID. Just complete the
        // sync right away. If the project was modified locally, but not saved,
        // the user will be prompted about the autosave.
        if (
            localProjectInfo->SnapshotId == snapshotInfo.Id
            && localProjectInfo->SyncStatus != sync::DBProjectData::SyncStatusDownloading) {
            CompleteSync(
                { sync::ProjectSyncResult::StatusCode::Succeeded, {}, utf8Path });
            return;
        }
        // 2. Project sync was interrupted.
        if (
            mode == SyncMode::Normal
            && localProjectInfo->SyncStatus == sync::DBProjectData::SyncStatusUploading) {
            // There is not enough information to decide if the project has
            // diverged. Just open it, so the sync can resume. If the project has
            // diverged, the user will be prompted to resolve the conflict on the
            // next save.
            CompleteSync(
                { sync::ProjectSyncResult::StatusCode::Succeeded, {}, utf8Path });
            return;
        }
        // 3. Project was modified locally, but not saved.
        if (HasAutosave(utf8Path)) {
            if (mode == SyncMode::Normal) {
                FailSync({ SyncResultCode::Conflict });
                return;
            } else {
                DropAutosave(utf8Path);
            }
        }
    }

    if (snapshotInfo.Id.empty()) {
        FailSync({ SyncResultCode::SyncImpossible });
        return;
    }

    mRemoteSnapshot = sync::RemoteProjectSnapshot::Sync(
        projectInfo, snapshotInfo, utf8Path,
        [this, createNew, path = utf8Path, projectId = projectInfo.Id,
         snapshotId = snapshotInfo.Id](sync::RemoteProjectSnapshotState state)
    {
        UpdateDowloadProgress(
            (state.ProjectDownloaded + state.BlocksDownloaded)
            / (state.BlocksTotal + 1.0));

        if (state.IsComplete()) {
            const bool success = state.Result.Code == SyncResultCode::Success;

            CompleteSync({ success
                           ? sync::ProjectSyncResult::StatusCode::Succeeded
                           : sync::ProjectSyncResult::StatusCode::Failed,
                           std::move(state.Result), std::move(path) });
        }
    },
        mode == SyncMode::ForceNew);
}

void CloudSyncService::UpdateDowloadProgress(double downloadProgress)
{
    mDownloadProgress.store(downloadProgress);

    if (mProgressUpdateQueued.exchange(true)) {
        return;
    }

    BasicUI::CallAfter(
        [this]
    {
        auto remoteSnapshot = mRemoteSnapshot;

        if (remoteSnapshot && !mProgressCallback(mDownloadProgress.load())) {
            remoteSnapshot->Cancel();
        }

        mProgressUpdateQueued.store(false);
    });
}

void CloudSyncService::ReportUploadStats(
    std::string_view projectId, const TransferStats& stats)
{
    sync::NetworkStats networkStats;

    networkStats.Bytes      = stats.BytesTransferred;
    networkStats.Blocks     = stats.BlocksTransferred;
    networkStats.Files      = stats.ProjectFilesTransferred;
    networkStats.Mixes      = 0;
    networkStats.IsDownload = true;

    using namespace audacity::network_manager;

    auto request = Request(GetServiceConfig().GetNetworkStatsUrl(projectId));

    request.setHeader(
        common_headers::ContentType, common_content_types::ApplicationJson);

    SetCommonHeaders(request);

    auto body = Serialize(networkStats);

    auto response
        =NetworkManager::GetInstance().doPost(request, body.data(), body.size());
    // Keep response alive
    response->setRequestFinishedCallback([response](auto) {});
}
} // namespace audacity::cloud::audiocom
