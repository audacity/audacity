/*
* Audacity: A Digital Audio Editor
*/
#include <thread>

#include "au3audiocomservice.h"

#include "au3-cloud-audiocom/CloudSyncService.h"
#include "au3-cloud-audiocom/OAuthService.h"
#include "au3-cloud-audiocom/ServiceConfig.h"
#include "au3-cloud-audiocom/sync/CloudSyncDTO.h"
#include "au3-cloud-audiocom/sync/ProjectCloudExtension.h"
#include "au3-cloud-audiocom/sync/LocalProjectSnapshot.h"
#include "au3-concurrency/concurrency/CancellationContext.h"
#include "au3-import-export/ExportUtils.h"
#include "au3-project-file-io/ProjectFileIO.h"

#include "au3cloud/cloudtypes.h"
#include "au3wrap/au3types.h"
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

        audioList.items.push_back(std::move(item));
    }

    audioList.meta.total = paginatedResponse.Pagination.TotalCount;
    audioList.meta.batches = paginatedResponse.Pagination.PagesCount;
    audioList.meta.thisBatchNumber = paginatedResponse.Pagination.CurrentPage;
    audioList.meta.itemsPerBatch = paginatedResponse.Pagination.PageSize;

    return audioList;
}
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

muse::ProgressPtr Au3AudioComService::uploadProject(au::project::IAudacityProjectPtr project, const std::string& name)
{
    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());

    muse::ProgressPtr progress = std::make_shared<muse::Progress>();

    auto& projectCloudExtension = audacity::cloud::audiocom::sync::ProjectCloudExtension::Get(*au3Project);
    m_projectUploadSubscription = projectCloudExtension.SubscribeStatusChanged(
        [this, progress](const audacity::cloud::audiocom::sync::CloudStatusChangedMessage& message) {
        if (message.Status == audacity::cloud::audiocom::sync::ProjectSyncStatus::Syncing) {
            progress->progress(message.Progress * 100, 100);
        }

        if (message.Status == audacity::cloud::audiocom::sync::ProjectSyncStatus::Synced) {
            progress->finish(muse::make_ret(muse::Ret::Code::Ok));
        }

        if (message.Status == audacity::cloud::audiocom::sync::ProjectSyncStatus::Failed) {
            progress->finish(muse::make_ret(muse::Ret::Code::UnknownError, std::string { "Project sync failed" }));
        }
    }, false);

    projectCloudExtension.OnSyncStarted();

    auto future = audacity::cloud::audiocom::sync::LocalProjectSnapshot::Create(
        audacity::cloud::audiocom::GetServiceConfig(),
        audacity::cloud::audiocom::GetOAuthService(),
        projectCloudExtension,
        name,
        audacity::cloud::audiocom::sync::UploadMode::Normal,
        AudiocomTrace::SaveProjectSaveToCloudMenu);

    std::thread([this, au3Project, future = std::move(future)]() mutable {
        auto result = future.get();

        if (!result.Response) {
            return;
        }

        auto& projectFileIO = ProjectFileIO::Get(*au3Project);
        projectFileIO.UpdateSaved(nullptr);
    }).detach();

    return progress;
}

std::string Au3AudioComService::getSharedAudioPage() const
{
    return m_sharedAudioUrl;
}

std::string Au3AudioComService::getCloudProjectPage(au::project::IAudacityProjectPtr project)
{
    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());

    auto& projectCloudExtension = audacity::cloud::audiocom::sync::ProjectCloudExtension::Get(*au3Project);
    return projectCloudExtension.GetCloudProjectPage(AudiocomTrace::SaveProjectSaveToCloudMenu);
}

muse::ProgressPtr Au3AudioComService::shareAudio(au::project::IAudacityProjectPtr, const std::string& title)
{
    muse::ProgressPtr progress = std::make_shared<muse::Progress>();

    std::thread([this, title, progress]() {
        auto formats = exporter()->formatsList();
        if (formats.empty()) {
            progress->finish(muse::make_ret(muse::Ret::Code::UnknownError, std::string { "No export formats available" }));
            return;
        }

        const auto it = std::find(formats.begin(), formats.end(), "WavPack Files");
        const std::string format = (it != formats.end()) ? *it : formats[0];
        exportConfiguration()->setCurrentFormat(format);
        exportConfiguration()->setProcessType(importexport::ExportProcessType::FULL_PROJECT_AUDIO);
        exportConfiguration()->setExportSampleRate(44100);
        exporter()->setValue(1, 24);

        auto extensions = exporter()->formatExtensions(format);
        std::string tempPath = "/tmp/teste." + extensions[0];

        const auto exportRet = exporter()->exportData(muse::io::path_t(tempPath), progress);
        if (!exportRet) {
            progress->finish(exportRet);
            return;
        }

        m_uploadService
            = std::make_unique<audacity::cloud::audiocom::UploadService>(audacity::cloud::audiocom::GetServiceConfig(),
                                                                         audacity::cloud::audiocom::GetOAuthService());
        const bool isPublic = false;
        m_uploadOperationHandle = m_uploadService->Upload(
            tempPath,
            wxString(title),
            isPublic,
            [this, progress, tempPath](const audacity::cloud::audiocom::UploadOperationCompleted& result) {
            if (wxFileExists(tempPath)) {
                wxRemoveFile(tempPath);
            }

            if (result.result == audacity::cloud::audiocom::UploadOperationCompleted::Result::Success) {
                auto* payload = std::get_if<audacity::cloud::audiocom::UploadSuccessfulPayload>(&result.payload);
                if (payload) {
                    m_sharedAudioUrl = payload->audioUrl;
                    progress->finish(muse::make_ret(muse::Ret::Code::Ok));
                }
            } else {
                progress->finish(muse::make_ret(muse::Ret::Code::UnknownError, std::string { "Upload failed" }));
            }
        },
            [progress](uint64_t current, uint64_t total) {
            // Progress 50-100% for upload
            double uploadProgress = double(current) / double(total);
            progress->progress(50 + uploadProgress * 50, 100);
        },
            AudiocomTrace::ShareAudioButton);
    }).detach();
    return progress;
}
