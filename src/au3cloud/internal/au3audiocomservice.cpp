/*
* Audacity: A Digital Audio Editor
*/
#include "au3audiocomservice.h"

#include <algorithm>
#include <atomic>
#include <chrono>
#include <cstdint>
#include <thread>
#include <utility>

#include "framework/global/async/async.h"
#include "framework/global/runtime.h"
#include "framework/global/types/ret.h"

#include "au3-cloud-audiocom/CloudSyncService.h"
#include "au3-cloud-audiocom/OAuthService.h"
#include "au3-cloud-audiocom/UserService.h"
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

#include "au3audiocomtypeconv.h"
#include "au3cloud/au3clouderrors.h"
#include "au3cloud/cloudtypes.h"
#include "au3wrap/au3types.h"
#include "importexport/export/iexporter.h"
#include "importexport/export/types/exporttypes.h"
#include "project/iaudacityproject.h"

using namespace au::au3cloud;
using namespace audacity::concurrency;
using namespace audacity::cloud::audiocom;

bool Au3AudioComService::enabled() const
{
    return true;
}

namespace {
muse::io::path_t getTempFileName(const muse::io::path_t tempDir, const std::string& ext)
{
    auto timestamp = std::chrono::system_clock::now().time_since_epoch().count();
    std::ostringstream oss;
    oss << timestamp;

    return tempDir
           .appendingComponent(oss.str())
           .appendingSuffix(ext);
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

        project->aboutCloseBegin().onNotify(this, [w_project = std::weak_ptr(project)]() {
            auto project = w_project.lock();
            if (!project) {
                return;
            }

            au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
            if (!au3Project) {
                return;
            }

            auto& projectCloudExtension = audacity::cloud::audiocom::sync::ProjectCloudExtension::Get(*au3Project);
            if (projectCloudExtension.IsSyncing()) {
                projectCloudExtension.CancelSync();
            }
        });
    });

    m_downloadManager->downloadCompleted().onReceive(this, [this](const std::string& audioId, const muse::io::path_t& path) {
        m_audioThumbnailFileUpdatedChannel.send(audioId, path);
    });

    authorization()->authState().ch.onReceive(this, [this](const AuthState& authState) {
        if (std::holds_alternative<NotAuthorized>(authState)) {
            clearAudioListCache();
            clearProjectListCache();
        }
    });

    appshellConfiguration()->aboutToRevertToFactorySettings().onNotify(this, []() {
        audacity::cloud::audiocom::sync::CloudProjectsDatabase::Get().CloseConnection();
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

    return muse::async::Promise<ProjectList>([weak = weak_from_this(), projectsPerBatch, batchNumber](const auto& resolve,
                                                                                                      const auto& reject) {
        std::thread([weak, projectsPerBatch, batchNumber, resolve, reject]() {
            auto self = weak.lock();
            if (!self) {
                (void)reject(static_cast<int>(muse::Ret::Code::UnknownError), "Service destroyed");
                return;
            }

            auto& cloudSyncService = CloudSyncService::Get();
            auto cancellationContext = audacity::concurrency::CancellationContext::Create();
            auto future = cloudSyncService.GetProjects(cancellationContext, batchNumber, projectsPerBatch, "");
            auto result = future.get();

            const auto* paginatedResponse = std::get_if<sync::PaginatedProjectsResponse>(&result);
            if (paginatedResponse) {
                const auto projects = convertFromAu3PaginatedProject(*paginatedResponse);
                {
                    std::lock_guard guard(self->m_cacheMutex);
                    self->m_projectListCache[batchNumber] = CachedProjectItem { projects, std::chrono::system_clock::now() };
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

muse::async::Channel<std::string, muse::io::path_t> Au3AudioComService::audioThumbnailFileUpdated() const
{
    return m_audioThumbnailFileUpdatedChannel;
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

    return muse::async::Promise<AudioList>([weak = weak_from_this(), audiosPerBatch, batchNumber](const auto& resolve, const auto& reject) {
        std::thread([weak, audiosPerBatch, batchNumber, resolve, reject]() {
            auto self = weak.lock();
            if (!self) {
                (void)reject(static_cast<int>(muse::Ret::Code::UnknownError), "Service destroyed");
                return;
            }

            auto& cloudSyncService = CloudSyncService::Get();
            auto cancellationContext = audacity::concurrency::CancellationContext::Create();
            auto future = cloudSyncService.GetAudioList(cancellationContext, batchNumber, audiosPerBatch, "");
            auto result = future.get();

            const auto* paginatedResponse = std::get_if<sync::PaginatedAudioResponse>(&result);
            if (paginatedResponse) {
                const auto audioList = convertFromAu3CloudAudio(*paginatedResponse, self->projectConfiguration()->cloudProjectsPath());
                {
                    std::lock_guard guard(self->m_cacheMutex);
                    self->m_audioListCache[batchNumber] = CachedAudioItem { audioList, std::chrono::system_clock::now() };
                }

                self->m_downloadManager->scheduleDownloads(convertToDownloadRequests(*paginatedResponse,
                                                                                     self->projectConfiguration()->cloudProjectsPath()));

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

muse::RetVal<muse::ProgressPtr> Au3AudioComService::uploadProject(au::project::IAudacityProjectPtr project, const std::string& name,
                                                                  std::function<bool()> projectSaveCallback, bool forceOverwrite)
{
    if (!project) {
        return muse::RetVal<muse::ProgressPtr>::make_ret(muse::Ret::Code::InternalError, muse::trc("cloud", "Invalid project"));
    }

    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
    if (!au3Project) {
        return muse::RetVal<muse::ProgressPtr>::make_ret(muse::Ret::Code::InternalError, muse::trc("cloud", "Invalid project"));
    }

    muse::ProgressPtr progress = std::make_shared<muse::Progress>();

    std::thread([weak = weak_from_this(), project, progress, name, forceOverwrite, projectSaveCallback = std::move(
                     projectSaveCallback)]() mutable {
        auto self = weak.lock();
        if (!self) {
            progress->finish(muse::make_ret(muse::Ret::Code::InternalError, muse::trc("cloud", "Service destroyed")));
            return;
        }

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

        if (projectCloudExtension.IsSyncing()) {
            projectCloudExtension.CancelSync();
        }

        projectCloudExtension.OnSyncStarted();

        const auto uploadMode = forceOverwrite
                                ? audacity::cloud::audiocom::sync::UploadMode::ForceOverwrite
                                : audacity::cloud::audiocom::sync::UploadMode::Normal;

        auto done = std::make_shared<std::atomic<bool> >(false);
        {
            std::lock_guard guard(self->m_uploadSubscriptionsMutex);

            self->m_projectUploadSubscriptions.erase(
                std::remove_if(
                    self->m_projectUploadSubscriptions.begin(),
                    self->m_projectUploadSubscriptions.end(),
                    [](const UploadSubscriptionEntry& e) { return e.done->load(); }),
                self->m_projectUploadSubscriptions.end());

            self->m_projectUploadSubscriptions.push_back(
                { projectCloudExtension.SubscribeStatusChanged(
                      [self, project, progress, done](
                          const audacity::cloud::audiocom::sync::CloudStatusChangedMessage& message) {
                    if (done->load()) {
                        return;
                    }

                    if (message.Status == audacity::cloud::audiocom::sync::ProjectSyncStatus::Syncing) {
                        progress->progress(message.Progress * 100, 100);
                    }

                    if (message.Status == audacity::cloud::audiocom::sync::ProjectSyncStatus::Synced) {
                        done->store(true);
                        progress->finish(muse::RetVal<muse::Val>::make_ok(muse::Val(self->getCloudProjectPage(project))));
                    }

                    if (message.Status == audacity::cloud::audiocom::sync::ProjectSyncStatus::Failed) {
                        done->store(true);
                        progress->finish(make_ret(cloudSyncErrorToErr(message.Error)));
                    }
                },
                      false),
                  done });
        }

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
            muse::async::Async::call(self.get(), [project, projectSaveCallback = std::move(projectSaveCallback)]() {
                bool ret = projectSaveCallback();
                if (ret) {
                    return;
                }
            }, muse::runtime::mainThreadId());

            au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
            if (!au3Project) {
                return;
            }

            auto& projectCloudExtension = audacity::cloud::audiocom::sync::ProjectCloudExtension::Get(*au3Project);
            projectCloudExtension.OnSyncCompleted(nullptr,
                                                  audacity::cloud::audiocom::sync::CloudSyncError {
                audacity::cloud::audiocom::sync::CloudSyncError::Aborted, muse::trc("project", "Could not save project locally") },
                                                  AudiocomTrace::SaveProjectSaveToCloudMenu);
        }
    }).detach();

    return muse::RetVal<muse::ProgressPtr>::make_ok(progress);
}

muse::RetVal<muse::ProgressPtr> Au3AudioComService::resumeProjectSync(au::project::IAudacityProjectPtr project)
{
    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
    auto& projectCloudExtension = audacity::cloud::audiocom::sync::ProjectCloudExtension::Get(*au3Project);

    const auto pendingSnapshots = sync::CloudProjectsDatabase::Get().GetPendingSnapshots(
        projectCloudExtension.GetCloudProjectId());

    if (pendingSnapshots.empty()) {
        return muse::RetVal<muse::ProgressPtr>::make_ok(nullptr);
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

    return muse::RetVal<muse::ProgressPtr>::make_ok(progress);
}

std::string Au3AudioComService::getCloudProjectPage(au::project::IAudacityProjectPtr project) const
{
    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());

    auto& projectCloudExtension = audacity::cloud::audiocom::sync::ProjectCloudExtension::Get(*au3Project);
    return projectCloudExtension.GetCloudProjectPage(AudiocomTrace::SaveProjectSaveToCloudMenu);
}

std::string Au3AudioComService::getCloudProjectPage(const std::string& slug) const
{
    auto& oauthService = GetOAuthService();
    const auto& serviceConfig = GetServiceConfig();

    const auto userId = GetUserService().GetUserId().ToStdString();
    const auto userslug = GetUserService().GetUserSlug().ToStdString();
    const auto projectPage = serviceConfig.GetProjectPagePath(userslug, slug, AudiocomTrace::OpenFromCloudMenu);
    return oauthService.MakeAudioComAuthorizeURL(userId, projectPage);
}

std::string Au3AudioComService::getCloudAudioPage(const std::string& slug) const
{
    auto& oauthService = GetOAuthService();
    const auto& serviceConfig = GetServiceConfig();

    const auto userId = GetUserService().GetUserId().ToStdString();
    const auto userslug = GetUserService().GetUserSlug().ToStdString();
    const auto audioPage = serviceConfig.GetAudioPagePath(userslug, slug, AudiocomTrace::OpenFromCloudMenu);
    return oauthService.MakeAudioComAuthorizeURL(userId, audioPage);
}

muse::RetVal<muse::ProgressPtr> Au3AudioComService::downloadAudioFile(const std::string& audioId)
{
    if (audioId.empty()) {
        return muse::RetVal<muse::ProgressPtr>::make_ret(muse::Ret::Code::UnknownError, muse::trc("cloud", "Invalid audio ID"));
    }

    muse::ProgressPtr progress = std::make_shared<muse::Progress>();

    auto progressCallback = [progress](double p) -> bool {
        if (progress->isCanceled()) {
            return false;
        }

        progress->progress(static_cast<int64_t>(p * 100), 100);
        return true;
    };

    auto cancellationContext = audacity::concurrency::CancellationContext::Create();
    auto future = CloudSyncService::Get().DownloadCloudAudio(audioId, std::move(progressCallback), cancellationContext);

    // Blocked is resolved synchronously (another download is already in progress)
    if (future.wait_for(std::chrono::seconds(0)) == std::future_status::ready) {
        const auto result = future.get();
        if (result.Status == sync::DownloadAudioResult::StatusCode::Blocked) {
            return muse::RetVal<muse::ProgressPtr>::make_ret(static_cast<int>(Err::DownloadAudioResultBlocked),
                                                             "Download already in progress");
        }
        return muse::RetVal<muse::ProgressPtr>::make_ret(muse::Ret::Code::UnknownError, "Failed to start audio download");
    }

    std::thread([future = std::move(future), progress]() mutable {
        const auto result = future.get();

        if (result.Status != sync::DownloadAudioResult::StatusCode::Succeeded) {
            if (result.Status == sync::DownloadAudioResult::StatusCode::Cancelled) {
                progress->finish(make_ret(Err::DownloadAudioResultCancelled));
            } else if (result.Status == sync::DownloadAudioResult::StatusCode::Failed) {
                progress->finish(make_ret(Err::DownloadAudioResultFailed));
            } else {
                progress->finish(make_ret(Err::UnknownError));
            }
            return;
        }

        progress->finish(muse::RetVal<muse::Val>::make_ok(muse::Val(muse::io::path_t(result.AudioPath))));
    }).detach();

    return muse::RetVal<muse::ProgressPtr>::make_ok(progress);
}

muse::RetVal<muse::ProgressPtr> Au3AudioComService::openCloudProject(const muse::io::path_t& localPath, const std::string& projectId,
                                                                     bool forceOverwrite)
{
    const auto dbProjectData = getProjectDataFromDatabase(localPath);
    if (dbProjectData.has_value() && !filesystem()->exists(localPath)) {
        removeProjectFromDatabase(localPath);
    }

    std::string cloudProjectId = projectId;

    if (cloudProjectId.empty()) {
        cloudProjectId = dbProjectData ? dbProjectData->ProjectId : "";

        if (cloudProjectId.empty()) {
            return muse::RetVal<muse::ProgressPtr>::make_ret(
                muse::Ret::Code::UnknownError, muse::trc("cloud", "Project not found in cloud database"));
        }
    }

    if (!forceOverwrite) {
        const auto unsyncedRet = checkUnsyncedProject(cloudProjectId);
        if (!unsyncedRet) {
            return muse::RetVal<muse::ProgressPtr>::make_ret(unsyncedRet);
        }
    }

    muse::ProgressPtr progress = std::make_shared<muse::Progress>();

    auto progressCallback = [progress](double p) -> bool {
        if (progress->isCanceled()) {
            return false;
        }

        progress->progress(static_cast<int64_t>(p * 100), 100);
        return true;
    };

    auto cancellationContext = audacity::concurrency::CancellationContext::Create();

    std::thread([weak = weak_from_this(), progress, dbProjectData, cloudProjectId, forceOverwrite,
                 cancellationContext, progressCallback = std::move(progressCallback)]() mutable {
        auto self = weak.lock();
        if (!self) {
            return;
        }

        auto cancelCheck = [progress](double) -> bool { return !progress->isCanceled(); };
        if (!forceOverwrite && self->isSnapshotUpToDate(dbProjectData, cancelCheck, cancellationContext)) {
            progress->finish(muse::make_ok());
            return;
        }

        const auto syncMode = forceOverwrite
                              ? CloudSyncService::SyncMode::ForceOverwrite
                              : CloudSyncService::SyncMode::Normal;
        const auto result = CloudSyncService::Get().OpenFromCloud(
            cloudProjectId, {}, syncMode, std::move(progressCallback), cancellationContext).get();

        if (result.Status == sync::ProjectSyncResult::StatusCode::Cancelled) {
            progress->finish(make_ret(Err::OpenProjectCancelled));
            return;
        }

        if (result.Status == sync::ProjectSyncResult::StatusCode::Succeeded) {
            progress->finish(muse::make_ok());
        } else {
            const auto err = syncResultCodeToErr(result.Result.Code);
            if (err == Err::SyncResultNotFound) {
                self->removeProjectFromDatabase(result.ProjectPath);
            }

            progress->finish(make_ret(err));
        }
    }).detach();

    return muse::RetVal<muse::ProgressPtr>::make_ok(progress);
}

muse::RetVal<muse::ProgressPtr> Au3AudioComService::shareAudio(const std::string& title)
{
    muse::ProgressPtr progress = std::make_shared<muse::Progress>();

    std::thread([weak = weak_from_this(), title, progress]() {
        auto self = weak.lock();
        if (!self) {
            progress->finish(muse::make_ret(muse::Ret::Code::InternalError, muse::trc("cloud", "Service destroyed")));
            return;
        }

        const auto preferredFormats = self->exporter()->cloudPreferredAudioFormats();
        if (preferredFormats.empty()) {
            progress->finish(make_ret(Err::NoExportPlugin));
            return;
        }

        auto project = self->globalContext()->currentProject();
        if (!project) {
            progress->finish(muse::make_ret(muse::Ret::Code::InternalError, muse::trc("cloud", "No valid current project")));
            return;
        }
        au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());

        const std::string format = preferredFormats[0];

        muse::ValList paramsList;
        for (const auto& [id, val] : self->exporter()->cloudExportParameters(format)) {
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

        const auto extensions = self->exporter()->formatExtensions(format);
        if (extensions.empty()) {
            progress->finish(make_ret(Err::NoExtensions));
            return;
        }

        const muse::io::path_t tempPath
            = getTempFileName(self->projectConfiguration()->temporaryDir(), extensions.front());

        const auto exportRet = self->exporter()->exportData(muse::io::path_t(tempPath), options, progress);
        if (!exportRet) {
            self->filesystem()->remove(tempPath);
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
            [op, progress, tempPath, filesystem = self->filesystem()](const audacity::cloud::audiocom::UploadOperationCompleted& result) {
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
    return muse::RetVal<muse::ProgressPtr>::make_ok(progress);
}

void Au3AudioComService::removeProjectFromDatabase(const muse::io::path_t& localPath)
{
    auto dbData = sync::CloudProjectsDatabase::Get().GetProjectDataForPath(localPath.toStdString());
    if (dbData) {
        sync::CloudProjectsDatabase::Get().DeleteProject(dbData->ProjectId);
    }
}

bool Au3AudioComService::isSnapshotUpToDate(
    const std::optional<sync::DBProjectData>& dbProjectData,
    sync::ProgressCallback progressCallback,
    CancellationContextPtr context)
{
    if (!dbProjectData.has_value()) {
        return false;
    }

    std::optional<std::string> headSnapshotId
        = getHeadSnapshotID(dbProjectData->ProjectId, progressCallback, context);
    if (!headSnapshotId.has_value()) {
        return false;
    }

    const bool snapshotsMatch = (*headSnapshotId == dbProjectData->SnapshotId);
    const bool fullyDownloaded = (dbProjectData->SyncStatus == sync::DBProjectData::SyncStatusSynced);
    return snapshotsMatch && fullyDownloaded;
}

std::optional<std::string> Au3AudioComService::getHeadSnapshotID(
    const std::string& projectId,
    sync::ProgressCallback progressCallback,
    CancellationContextPtr context)
{
    const auto headResult
        = CloudSyncService::Get().GetHeadSnapshotID(projectId, progressCallback, std::move(context)).get();
    if (const auto* snapshotId = std::get_if<std::string>(&headResult)) {
        return *snapshotId;
    }

    return std::nullopt;
}

std::optional<ProjectList::Item> Au3AudioComService::findCachedProject(const std::string& projectId) const
{
    std::lock_guard guard(m_cacheMutex);
    for (const auto& [_, cached] : m_projectListCache) {
        for (const auto& item : cached.projectList.items) {
            if (item.id == projectId) {
                return item;
            }
        }
    }
    return std::nullopt;
}

muse::Ret Au3AudioComService::checkUnsyncedProject(const std::string& cloudProjectId) const
{
    const auto cachedProject = findCachedProject(cloudProjectId);
    if (!cachedProject || cachedProject->headSnapshotSynced != 0) {
        return muse::make_ok();
    }

    const auto localState = CloudSyncService::GetProjectState(cloudProjectId);
    if (localState == CloudSyncService::ProjectState::PendingSync) {
        return muse::make_ok();
    }

    const bool hasValidSnapshot = !cachedProject->lastSyncedSnapshotId.empty();
    return make_ret(hasValidSnapshot ? Err::CloudProjectNotFullySynced : Err::CloudProjectNeverSynced);
}

void Au3AudioComService::deinit()
{
    audacity::cloud::audiocom::sync::CloudProjectsDatabase::Get().CloseConnection();
}
