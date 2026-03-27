/*
* Audacity: A Digital Audio Editor
*/
#include "au3audiocomservice.h"

#include <chrono>
#include <cstdint>
#include <thread>

#include "framework/global/async/async.h"
#include "framework/global/runtime.h"
#include "framework/global/types/ret.h"

#include "au3-cloud-audiocom/CloudSyncService.h"
#include "au3-cloud-audiocom/OAuthService.h"
#include "au3-cloud-audiocom/ServiceConfig.h"
#include "au3-cloud-audiocom/sync/CloudSyncDTO.h"
#include "au3-cloud-audiocom/sync/CloudProjectsDatabase.h"
#include "au3-cloud-audiocom/sync/ProjectCloudExtension.h"
#include "au3-cloud-audiocom/sync/LocalProjectSnapshot.h"
#include "au3-cloud-audiocom/sync/ResumedSnaphotUploadOperation.h"
#include "au3-cloud-audiocom/UploadService.h"
#include "au3-concurrency/concurrency/CancellationContext.h"
#include "au3-import-export/ExportUtils.h"
#include "au3-project-rate/ProjectRate.h"

#include "au3cloud/au3clouderrors.h"
#include "au3cloud/cloudtypes.h"
#include "au3wrap/au3types.h"
#include "importexport/export/iexporter.h"
#include "importexport/export/types/exporttypes.h"
#include "project/iaudacityproject.h"

using namespace au::au3cloud;
using namespace audacity::concurrency;
using namespace audacity::cloud::audiocom;

namespace {
au::au3cloud::ProjectList convertFromAu3PaginatedProject(const sync::PaginatedProjectsResponse& paginatedResponse)
{
    au::au3cloud::ProjectList projectList;

    for (size_t i = 0; i < paginatedResponse.Items.size(); i++) {
        const auto& projectInfo = paginatedResponse.Items[i];

        au::au3cloud::ProjectList::Item item;
        item.id = projectInfo.Id;
        item.name = projectInfo.Name;
        item.slug = projectInfo.Slug;
        item.authorName = projectInfo.AuthorName;
        item.username = projectInfo.Username;
        item.details = projectInfo.Details;
        item.lastSyncedSnapshotId = projectInfo.LastSyncedSnapshotId;
        item.created = projectInfo.Created;
        item.updated = projectInfo.Updated;
        item.fileSize = projectInfo.HeadSnapshot.FileSize;

        projectList.items.push_back(std::move(item));
    }

    projectList.meta.total = paginatedResponse.Pagination.TotalCount;
    projectList.meta.batches = paginatedResponse.Pagination.PagesCount;
    projectList.meta.thisBatchNumber = paginatedResponse.Pagination.CurrentPage;
    projectList.meta.itemsPerBatch = paginatedResponse.Pagination.PageSize;

    return projectList;
}

au::au3cloud::AudioList convertFromAu3CloudAudio(const sync::PaginatedAudioResponse& paginatedResponse)
{
    au::au3cloud::AudioList audioList;

    for (size_t i = 0; i < paginatedResponse.Items.size(); i++) {
        const auto& audioInfo = paginatedResponse.Items[i];

        au::au3cloud::AudioList::Item item;
        item.id = audioInfo.Id;
        item.username = audioInfo.Username;
        item.authorName = audioInfo.AuthorName;
        item.slug = audioInfo.Slug;
        item.title = audioInfo.Title;
        item.tags = audioInfo.Tags;
        item.created = audioInfo.Created;
        item.fileSize = audioInfo.FileSize;
        item.duration = audioInfo.Duration;

        audioList.items.push_back(std::move(item));
    }

    audioList.meta.total = paginatedResponse.Pagination.TotalCount;
    audioList.meta.batches = paginatedResponse.Pagination.PagesCount;
    audioList.meta.thisBatchNumber = paginatedResponse.Pagination.CurrentPage;
    audioList.meta.itemsPerBatch = paginatedResponse.Pagination.PageSize;

    return audioList;
}

muse::io::path_t getTempFileName(const muse::io::path_t tempDir, const std::string& ext)
{
    auto timestamp = std::chrono::system_clock::now().time_since_epoch().count();
    std::ostringstream oss;
    oss << timestamp;

    return tempDir
           .appendingComponent(oss.str())
           .appendingSuffix(ext);
}

au::au3cloud::Err cloudSyncErrorToErr(const std::optional<sync::CloudSyncError>& error)
{
    if (!error.has_value()) {
        return Err::UnknownError;
    }

    using ErrorType = sync::CloudSyncError::ErrorType;
    switch (error->Type) {
    case ErrorType::None:
        return Err::UnknownError;
    case ErrorType::Authorization:
        return Err::ProjectForbidden;
    case ErrorType::ProjectLimitReached:
        return Err::ProjectLimitReached;
    case ErrorType::ProjectStorageLimitReached:
        return Err::ProjectStorageLimitReached;
    case ErrorType::ProjectVersionConflict:
        return Err::ProjectVersionConflict;
    case ErrorType::ProjectNotFound:
        return Err::ProjectNotFound;
    case ErrorType::DataUploadFailed:
        return Err::DataUploadFailed;
    case ErrorType::Network:
        return Err::NetworkError;
    case ErrorType::Server:
        return Err::ServerError;
    case ErrorType::Cancelled:
        return Err::SyncCancelled;
    case ErrorType::Aborted:
        return Err::SyncAborted;
    case ErrorType::ClientFailure:
        return Err::ClientFailure;
    }

    return Err::UnknownError;
}

au::au3cloud::Err uploadResultToErr(UploadOperationCompleted::Result result)
{
    using Result = UploadOperationCompleted::Result;
    switch (result) {
    case Result::Success:
        return Err::NoError;
    case Result::Aborted:
        return Err::UploadAborted;
    case Result::FileNotFound:
        return Err::UploadFileNotFound;
    case Result::Unauthorized:
        return Err::UploadUnauthorized;
    case Result::InvalidData:
        return Err::UploadInvalidData;
    case Result::UnexpectedResponse:
        return Err::UploadUnexpectedResponse;
    case Result::UploadFailed:
        return Err::UploadFailed;
    }

    return Err::UnknownError;
}

au::au3cloud::Err syncResultCodeToErr(audacity::cloud::audiocom::SyncResultCode code)
{
    switch (code) {
    case SyncResultCode::Success:
        return Err::NoError;
    case SyncResultCode::Cancelled:
        return Err::SyncResultCancelled;
    case SyncResultCode::Expired:
        return Err::SyncResultExpired;
    case SyncResultCode::Conflict:
        return Err::SyncResultConflict;
    case SyncResultCode::ConnectionFailed:
        return Err::SyncResultConnectionFailed;
    case SyncResultCode::PaymentRequired:
        return Err::SyncResultPaymentRequired;
    case SyncResultCode::TooLarge:
        return Err::SyncResultTooLarge;
    case SyncResultCode::Unauthorized:
        return Err::SyncResultUnauthorized;
    case SyncResultCode::Forbidden:
        return Err::SyncResultForbidden;
    case SyncResultCode::NotFound:
        return Err::SyncResultNotFound;
    case SyncResultCode::UnexpectedResponse:
        return Err::SyncResultUnexpectedResponse;
    case SyncResultCode::InternalClientError:
        return Err::SyncResultInternalClientError;
    case SyncResultCode::InternalServerError:
        return Err::SyncResultInternalServerError;
    case SyncResultCode::SyncImpossible:
        return Err::SyncResultSyncImpossible;
    case SyncResultCode::UnknownError:
        return Err::UnknownError;
    }

    return Err::UnknownError;
}

std::optional<sync::DBProjectData> getProjectDataFromDatabase(const muse::io::path_t& localPath)
{
    return sync::CloudProjectsDatabase::Get().GetProjectDataForPath(localPath.toStdString());
}
}

void Au3AudioComService::init()
{
    globalContext()->currentProjectChanged().onNotify(this, [this]() {
        auto project = globalContext()->currentProject();
        if (!project) {
            return;
        }

        project->aboutCloseBegin().onNotify(this, [project]() {
            au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
            if (!au3Project) {
                return;
            }

            auto& projectCloudExtension = audacity::cloud::audiocom::sync::ProjectCloudExtension::Get(*au3Project);
            if (projectCloudExtension.IsCloudProject()) {
                projectCloudExtension.CancelSync();
            }
        });
    });
}

muse::async::Promise<ProjectList> Au3AudioComService::downloadProjectList(size_t projectsPerBatch, size_t batchNumber,
                                                                          const FetchOptions& options)
{
    if (m_projectsPerBatch != projectsPerBatch) {
        std::lock_guard guard(m_cacheMutex);
        m_projectListCache.clear();
        m_projectsPerBatch = projectsPerBatch;
    }

    if (options.cachePolicy == CachePolicy::CacheFirst) {
        std::optional<CachedProjectItem> cacheProjectOpt = {};
        {
            std::lock_guard guard(m_cacheMutex);
            if (auto iter = m_projectListCache.find(batchNumber); iter != m_projectListCache.end()) {
                cacheProjectOpt = iter->second;
            }
        }

        if (cacheProjectOpt.has_value()) {
            const auto& cachedProject = cacheProjectOpt.value();
            if (options.maxCacheAge.has_value()) {
                const auto now = std::chrono::system_clock::now();
                const auto cacheAge = now - cachedProject.timestamp;
                if (options.maxCacheAge.value() > cacheAge) {
                    return muse::async::Promise<ProjectList>([cachedProject](const auto& resolve, const auto&) {
                        return resolve(cachedProject.projectList);
                    });
                }
            } else {
                return muse::async::Promise<ProjectList>([cachedProject](const auto& resolve, const auto&) {
                    return resolve(cachedProject.projectList);
                });
            }
        }
    }

    return muse::async::Promise<ProjectList>([this, projectsPerBatch, batchNumber](const auto& resolve, const auto& reject) {
        std::thread([this, projectsPerBatch, batchNumber, resolve, reject]() {
            auto& cloudSyncService = CloudSyncService::Get();
            auto cancellationContext = audacity::concurrency::CancellationContext::Create();
            auto future = cloudSyncService.GetProjects(cancellationContext, batchNumber, projectsPerBatch, "");
            auto result = future.get();

            const auto* paginatedResponse = std::get_if<sync::PaginatedProjectsResponse>(&result);
            if (paginatedResponse) {
                const auto projects = convertFromAu3PaginatedProject(*paginatedResponse);
                {
                    std::lock_guard guard(m_cacheMutex);
                    m_projectListCache[batchNumber] = CachedProjectItem { projects, std::chrono::system_clock::now() };
                }

                (void)resolve(projects);
            } else {
                (void)reject(static_cast<int>(muse::Ret::Code::UnknownError), "Failed to fetch project list from cloud service");
            }
        }).detach();

        return muse::async::Promise<ProjectList>::dummy_result();
    }, muse::async::PromiseType::AsyncByBody);
}

void Au3AudioComService::clearProjectListCache()
{
    auto guard = std::lock_guard(m_cacheMutex);
    m_projectListCache.clear();
    m_projectsPerBatch = 0;
}

muse::async::Promise<AudioList> Au3AudioComService::downloadAudioList(size_t audiosPerBatch, size_t batchNumber,
                                                                      const FetchOptions& options)
{
    if (m_audiosPerBatch != audiosPerBatch) {
        auto guard = std::lock_guard(m_cacheMutex);
        m_audioListCache.clear();
        m_audiosPerBatch = audiosPerBatch;
    }

    if (options.cachePolicy == CachePolicy::CacheFirst) {
        std::optional<CachedAudioItem> cachedAudioOpt = {};
        {
            std::lock_guard guard(m_cacheMutex);
            if (auto iter = m_audioListCache.find(batchNumber); iter != m_audioListCache.end()) {
                cachedAudioOpt = iter->second;
            }
        }

        if (cachedAudioOpt.has_value()) {
            const auto& cachedAudio = cachedAudioOpt.value();
            if (options.maxCacheAge.has_value()) {
                const auto now = std::chrono::system_clock::now();
                const auto cacheAge = now - cachedAudio.timestamp;
                if (options.maxCacheAge.value() > cacheAge) {
                    return muse::async::Promise<AudioList>([cachedAudio](auto resolve, auto) {
                        return resolve(cachedAudio.audioList);
                    });
                }
            } else {
                return muse::async::Promise<AudioList>([cachedAudio](auto resolve, auto) {
                    return resolve(cachedAudio.audioList);
                });
            }
        }
    }

    return muse::async::Promise<AudioList>([this, audiosPerBatch, batchNumber](const auto& resolve, const auto& reject) {
        std::thread([this, audiosPerBatch, batchNumber, resolve, reject]() {
            auto& cloudSyncService = CloudSyncService::Get();
            auto cancellationContext = audacity::concurrency::CancellationContext::Create();
            auto future = cloudSyncService.GetAudioList(cancellationContext, batchNumber, audiosPerBatch, "");
            auto result = future.get();

            const auto* paginatedResponse = std::get_if<sync::PaginatedAudioResponse>(&result);
            if (paginatedResponse) {
                const auto audioList = convertFromAu3CloudAudio(*paginatedResponse);
                {
                    std::lock_guard guard(m_cacheMutex);
                    m_audioListCache[batchNumber] = CachedAudioItem { audioList, std::chrono::system_clock::now() };
                }

                (void)resolve(audioList);
            } else {
                (void)reject(static_cast<int>(muse::Ret::Code::UnknownError), "Failed to fetch audio list from cloud service");
            }
        }).detach();

        return muse::async::Promise<AudioList>::dummy_result();
    }, muse::async::PromiseType::AsyncByBody);
}

void Au3AudioComService::clearAudioListCache()
{
    std::lock_guard guard(m_cacheMutex);
    m_audioListCache.clear();
    m_audiosPerBatch = 0;
}

muse::ProgressPtr Au3AudioComService::uploadProject(au::project::IAudacityProjectPtr project, const std::string& name,
                                                    std::function<bool()> projectSaveCallback, bool forceOverwrite)
{
    muse::ProgressPtr progress = std::make_shared<muse::Progress>();

    if (!project) {
        progress->finish(muse::make_ret(muse::Ret::Code::InternalError, muse::trc("cloud", "Invalid project")));
        return progress;
    }

    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
    if (!au3Project) {
        progress->finish(muse::make_ret(muse::Ret::Code::InternalError, muse::trc("cloud", "Invalid project")));
        return progress;
    }

    auto& projectCloudExtension = audacity::cloud::audiocom::sync::ProjectCloudExtension::Get(*au3Project);
    m_projectUploadSubscription = projectCloudExtension.SubscribeStatusChanged(
        [this, project, progress](const audacity::cloud::audiocom::sync::CloudStatusChangedMessage& message) {
        if (message.Status == audacity::cloud::audiocom::sync::ProjectSyncStatus::Syncing) {
            progress->progress(message.Progress * 100, 100);
        }

        if (message.Status == audacity::cloud::audiocom::sync::ProjectSyncStatus::Synced) {
            progress->finish(muse::RetVal<muse::Val>::make_ok(muse::Val(getCloudProjectPage(project))));
        }

        if (message.Status == audacity::cloud::audiocom::sync::ProjectSyncStatus::Failed) {
            progress->finish(make_ret(cloudSyncErrorToErr(message.Error)));
        }
    }, false);

    const bool isSyncing = projectCloudExtension.IsSyncing();
    const std::string currentSnapshotId = projectCloudExtension.GetSnapshotId();
    const std::string cloudProjectId = projectCloudExtension.GetCloudProjectId();

    std::thread([this, project, progress, name, isSyncing, currentSnapshotId, cloudProjectId, forceOverwrite,
                 projectSaveCallback = std::move(projectSaveCallback)]() mutable {
        if (!project) {
            progress->finish(muse::make_ret(muse::Ret::Code::InternalError, muse::trc("cloud",
                                                                                      "Project was closed before upload started")));
            return;
        }

        au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
        if (!au3Project) {
            progress->finish(muse::make_ret(muse::Ret::Code::InternalError, muse::trc("cloud", "Invalid project")));
            return;
        }

        auto& projectCloudExtension = audacity::cloud::audiocom::sync::ProjectCloudExtension::Get(*au3Project);

        if (isSyncing) {
            const auto headSnapshotId = getHeadSnapshotID(cloudProjectId);
            if (headSnapshotId.has_value() && *headSnapshotId == currentSnapshotId) {
                // Already syncing the current snapshot
                return;
            }

            muse::async::Async::call(this, [&projectCloudExtension]() {
                projectCloudExtension.CancelSync();
            }, muse::runtime::mainThreadId());
        }

        projectCloudExtension.OnSyncStarted();
        const auto uploadMode = forceOverwrite
                                ? audacity::cloud::audiocom::sync::UploadMode::ForceOverwrite
                                : audacity::cloud::audiocom::sync::UploadMode::Normal;
        auto future = audacity::cloud::audiocom::sync::LocalProjectSnapshot::Create(
            audacity::cloud::audiocom::GetServiceConfig(),
            audacity::cloud::audiocom::GetOAuthService(),
            projectCloudExtension,
            name,
            uploadMode,
            AudiocomTrace::SaveProjectSaveToCloudMenu);

        auto result = future.get();

        if (!result.Response) {
            return;
        }

        if (projectSaveCallback) {
            muse::async::Async::call(this, [project, projectSaveCallback = std::move(projectSaveCallback)]() {
                bool ret = projectSaveCallback();
                if (ret) {
                    return;
                }

                au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
                if (!au3Project) {
                    return;
                }

                auto& projectCloudExtension = audacity::cloud::audiocom::sync::ProjectCloudExtension::Get(*au3Project);
                projectCloudExtension.OnSyncCompleted(nullptr,
                                                      audacity::cloud::audiocom::sync::CloudSyncError {
                    audacity::cloud::audiocom::sync::CloudSyncError::Aborted, muse::trc("project", "Could not save project locally") },
                                                      AudiocomTrace::SaveProjectSaveToCloudMenu);
            }, muse::runtime::mainThreadId());
        }
    }).detach();

    return progress;
}

muse::ProgressPtr Au3AudioComService::resumeProjectSync(au::project::IAudacityProjectPtr project)
{
    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
    auto& projectCloudExtension = audacity::cloud::audiocom::sync::ProjectCloudExtension::Get(*au3Project);

    const auto pendingSnapshots = sync::CloudProjectsDatabase::Get().GetPendingSnapshots(
        projectCloudExtension.GetCloudProjectId());

    if (pendingSnapshots.empty()) {
        return nullptr;
    }

    auto progress = std::make_shared<muse::Progress>();
    m_resumeSyncSubscription = projectCloudExtension.SubscribeStatusChanged(
        [progress](const audacity::cloud::audiocom::sync::CloudStatusChangedMessage& message) {
        if (message.Status == audacity::cloud::audiocom::sync::ProjectSyncStatus::Syncing) {
            progress->progress(message.Progress * 100, 100);
        }

        if (message.Status == audacity::cloud::audiocom::sync::ProjectSyncStatus::Synced) {
            progress->finish(muse::make_ok());
        }

        if (message.Status == audacity::cloud::audiocom::sync::ProjectSyncStatus::Failed) {
            progress->finish(make_ret(cloudSyncErrorToErr(message.Error)));
        }
    }, false);

    audacity::cloud::audiocom::sync::ResumeProjectUpload(projectCloudExtension, {});

    return progress;
}

std::string Au3AudioComService::getCloudProjectPage(au::project::IAudacityProjectPtr project)
{
    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());

    auto& projectCloudExtension = audacity::cloud::audiocom::sync::ProjectCloudExtension::Get(*au3Project);
    return projectCloudExtension.GetCloudProjectPage(AudiocomTrace::SaveProjectSaveToCloudMenu);
}

muse::ProgressPtr Au3AudioComService::openCloudProject(const muse::io::path_t& localPath, const std::string& projectId,
                                                       bool forceOverwrite)
{
    muse::ProgressPtr progress = std::make_shared<muse::Progress>();

    const auto dbProjectData = getProjectDataFromDatabase(localPath);
    if (dbProjectData.has_value() && !filesystem()->exists(localPath)) {
        removeProjectFromDatabase(localPath);
    }

    std::string cloudProjectId = projectId;

    if (cloudProjectId.empty()) {
        cloudProjectId = dbProjectData ? dbProjectData->ProjectId : "";

        if (cloudProjectId.empty()) {
            progress->finish(muse::make_ret(muse::Ret::Code::UnknownError, muse::trc("cloud", "Project not found in cloud database")));
            return progress;
        }
    }

    std::thread([this, progress, dbProjectData, path = localPath.toStdString(), cloudProjectId, forceOverwrite]() {
        if (!forceOverwrite && isSnapshotUpToDate(dbProjectData)) {
            progress->finish(muse::make_ok());
            return;
        }

        auto progressCallback = [progress](double p) -> bool {
            if (progress->isCanceled()) {
                return false;
            }

            progress->progress(static_cast<int64_t>(p * 100), 100);
            return true;
        };

        using SyncFuture = CloudSyncService::SyncFuture;
        auto syncFuturePromise = std::make_shared<std::promise<SyncFuture> >();
        std::future<SyncFuture> syncFutureResult = syncFuturePromise->get_future();

        muse::async::Async::call(this, [syncFuturePromise, cloudProjectId, forceOverwrite,
                                        progressCallback = std::move(progressCallback)]() mutable {
            // Important: CloudSyncService does not control access by locking but by calling convention,
            // We must ensure all operations on this service to be called on the main thread
            const auto syncMode = forceOverwrite
                                  ? CloudSyncService::SyncMode::ForceOverwrite
                                  : CloudSyncService::SyncMode::Normal;
            auto future = CloudSyncService::Get().OpenFromCloud(
                cloudProjectId, {},
                syncMode,
                std::move(progressCallback));

            syncFuturePromise->set_value(std::move(future));
        }, muse::runtime::mainThreadId());

        auto syncFuture = syncFutureResult.get();
        auto result = syncFuture.get();

        if (progress->isCanceled()) {
            return;
        }

        muse::async::Async::call(this, [this, progress, result = std::move(result)]() {
            if (result.Status == sync::ProjectSyncResult::StatusCode::Succeeded) {
                progress->finish(muse::make_ok());
            } else {
                const auto err = syncResultCodeToErr(result.Result.Code);
                if (err == Err::SyncResultNotFound) {
                    removeProjectFromDatabase(result.ProjectPath);
                }

                progress->finish(make_ret(err));
            }
        }, muse::runtime::mainThreadId());
    }).detach();

    return progress;
}

muse::ProgressPtr Au3AudioComService::shareAudio(const std::string& title)
{
    muse::ProgressPtr progress = std::make_shared<muse::Progress>();

    std::thread([this, title, progress]() {
        const auto preferredFormats = exporter()->cloudPreferredAudioFormats();
        if (preferredFormats.empty()) {
            progress->finish(make_ret(Err::NoExportPlugin));
            return;
        }

        auto project = globalContext()->currentProject();
        if (!project) {
            progress->finish(muse::make_ret(muse::Ret::Code::InternalError, muse::trc("cloud", "No valid current project")));
            return;
        }
        au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());

        const std::string format = preferredFormats[0];

        muse::ValList paramsList;
        for (const auto& [id, val] : exporter()->cloudExportParameters(format)) {
            muse::ValMap entry;
            entry["id"] = muse::Val(id);
            entry["value"] = std::visit([](auto v) -> muse::Val { return muse::Val(v); }, val);
            paramsList.push_back(muse::Val(entry));
        }

        importexport::IExporter::Options options;
        options[importexport::IExporter::OptionKey::Format] = muse::Val(format);
        options[importexport::IExporter::OptionKey::ProcessType] = muse::Val(importexport::ExportProcessType::FULL_PROJECT_AUDIO);
        options[importexport::IExporter::OptionKey::ExportChannelsType]
            = muse::Val(static_cast<int>(importexport::ExportChannelsPref::ExportChannels::STEREO));
        options[importexport::IExporter::OptionKey::ExportChannels] = muse::Val(2);
        options[importexport::IExporter::OptionKey::ExportSampleRate]
            = muse::Val(static_cast<int>(ProjectRate::Get(*au3Project).GetRate()));
        options[importexport::IExporter::OptionKey::Parameters] = muse::Val(paramsList);

        const auto extensions = exporter()->formatExtensions(format);
        if (extensions.empty()) {
            progress->finish(make_ret(Err::NoExtensions));
            return;
        }

        const muse::io::path_t tempPath
            = getTempFileName(projectConfiguration()->temporaryDir(), extensions.front());

        const auto exportRet = exporter()->exportData(muse::io::path_t(tempPath), options, progress);
        if (!exportRet) {
            filesystem()->remove(tempPath);
            progress->finish(exportRet);
            return;
        }

        struct UploadOp {
            std::shared_ptr<audacity::cloud::audiocom::UploadService> service;
            audacity::cloud::audiocom::UploadOperationHandle handle;
        };

        auto op = std::make_shared<UploadOp>();
        op->service = std::make_shared<audacity::cloud::audiocom::UploadService>(
            audacity::cloud::audiocom::GetServiceConfig(),
            audacity::cloud::audiocom::GetOAuthService());

        const bool isPublic = false;
        op->handle = op->service->Upload(
            tempPath.toStdString(),
            title,
            isPublic,
            [op, progress, tempPath, filesystem = filesystem()](const audacity::cloud::audiocom::UploadOperationCompleted& result) {
            filesystem->remove(tempPath);

            if (result.result == audacity::cloud::audiocom::UploadOperationCompleted::Result::Success) {
                auto* payload = std::get_if<audacity::cloud::audiocom::UploadSuccessfulPayload>(&result.payload);
                if (!payload) {
                    progress->finish(muse::make_ret(muse::Ret::Code::InternalError,
                                                    muse::trc("cloud", "Upload succeeded but payload is missing")));
                    return;
                }
                progress->finish(muse::RetVal<muse::Val>::make_ok(muse::Val(payload->audioUrl)));
            } else {
                progress->finish(make_ret(uploadResultToErr(result.result)));
            }
        },
            [progress](uint64_t current, uint64_t total) {
            progress->progress(static_cast<int64_t>(current), static_cast<int64_t>(total));
        },
            AudiocomTrace::ShareAudioButton);
    }).detach();
    return progress;
}

void Au3AudioComService::removeProjectFromDatabase(const muse::io::path_t& localPath)
{
    auto dbData = sync::CloudProjectsDatabase::Get().GetProjectDataForPath(localPath.toStdString());
    if (dbData) {
        sync::CloudProjectsDatabase::Get().DeleteProject(dbData->ProjectId);
    }
}

bool Au3AudioComService::isSnapshotUpToDate(const std::optional<sync::DBProjectData>& dbProjectData)
{
    if (!dbProjectData.has_value()) {
        return false;
    }

    std::optional<std::string> headSnapshotId = getHeadSnapshotID(dbProjectData->ProjectId);
    if (!headSnapshotId.has_value()) {
        return false;
    }

    const bool snapshotsMatch = (*headSnapshotId == dbProjectData->SnapshotId);
    const bool fullyDownloaded = (dbProjectData->SyncStatus == sync::DBProjectData::SyncStatusSynced);
    return snapshotsMatch && fullyDownloaded;
}

std::optional<std::string> Au3AudioComService::getHeadSnapshotID(const std::string& projectId)
{
    using HeadFuture = CloudSyncService::GetHeadSnapshotIDFuture;
    auto headPromise = std::make_shared<std::promise<HeadFuture> >();
    std::future<HeadFuture> headFutureResult = headPromise->get_future();

    muse::async::Async::call(this, [headPromise, projectId]() {
        // Important: CloudSyncService does not control access by locking but by calling convention,
        // We must ensure all operations on this service to be called on the main thread
        headPromise->set_value(CloudSyncService::Get().GetHeadSnapshotID(projectId));
    }, muse::runtime::mainThreadId());

    auto headFuture = headFutureResult.get();
    auto headResult = headFuture.get();

    if (const auto* snapshotId = std::get_if<std::string>(&headResult)) {
        return *snapshotId;
    }

    return std::nullopt;
}
