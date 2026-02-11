/*
* Audacity: A Digital Audio Editor
*/

#include "au3audiocomservice.h"

#include "au3-cloud-audiocom/CloudSyncService.h"
#include "au3-cloud-audiocom/sync/CloudSyncDTO.h"
#include "au3cloud/cloudtypes.h"

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
        m_projectListCache.clear();
        m_projectsPerBatch = projectsPerBatch;
    }

    if (options.cachePolicy == CachePolicy::CacheFirst) {
        if (auto iter = m_projectListCache.find(batchNumber); iter != m_projectListCache.end()) {
            const auto& cachedProject = iter->second;
            if (options.maxCacheAge.has_value()) {
                const auto now = std::chrono::system_clock::now();
                const auto cacheAge = now - cachedProject.timestamp;
                if (options.maxCacheAge.value() > cacheAge) {
                    return muse::async::Promise<ProjectList>([cachedProject](auto resolve, auto) {
                        return resolve(cachedProject.projectList);
                    });
                }
            } else {
                return muse::async::Promise<ProjectList>([cachedProject](auto resolve, auto) {
                    return resolve(cachedProject.projectList);
                });
            }
        }
    }

    return muse::async::Promise<ProjectList>([this, projectsPerBatch, batchNumber](auto resolve, auto) {
        auto& cloudSyncService = CloudSyncService::Get();
        auto cancellationContext = CancellationContext::Create();
        auto future = cloudSyncService.GetProjects(cancellationContext, 1, 20, "");
        auto result = future.get();

        const auto* paginatedResponse = std::get_if<sync::PaginatedProjectsResponse>(&result);
        if (paginatedResponse) {
            const auto projects = convertFromAu3PaginatedProject(*paginatedResponse);
            m_projectListCache[batchNumber] = CachedProjectItem { projects, std::chrono::system_clock::now() };
            (void)resolve(projects);
        } else {
            ProjectList projects;
            (void)resolve(projects);
        }

        return muse::async::Promise<ProjectList>::dummy_result();
    });
}

void Au3AudioComService::clearProjectListCache()
{
    m_projectListCache.clear();
    m_projectsPerBatch = 0;
}

muse::async::Promise<AudioList> Au3AudioComService::downloadAudioList(size_t audiosPerBatch, size_t batchNumber,
                                                                      const FetchOptions& options)
{
    if (m_audiosPerBatch != audiosPerBatch) {
        m_audioListCache.clear();
        m_audiosPerBatch = audiosPerBatch;
    }

    if (options.cachePolicy == CachePolicy::CacheFirst) {
        if (auto iter = m_audioListCache.find(batchNumber); iter != m_audioListCache.end()) {
            const auto& cachedAudio = iter->second;
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

    return muse::async::Promise<AudioList>([this, audiosPerBatch, batchNumber](auto resolve, auto) {
        auto& cloudSyncService = CloudSyncService::Get();
        auto cancellationContext = CancellationContext::Create();
        auto future = cloudSyncService.GetAudioList(cancellationContext, batchNumber, audiosPerBatch, "");
        auto result = future.get();

        const auto* paginatedResponse = std::get_if<sync::PaginatedAudioResponse>(&result);
        if (paginatedResponse) {
            const auto audioList = convertFromAu3CloudAudio(*paginatedResponse);
            m_audioListCache[batchNumber] = CachedAudioItem { audioList, std::chrono::system_clock::now() };
            (void)resolve(audioList);
        } else {
            AudioList audioList;
            (void)resolve(audioList);
        }

        return muse::async::Promise<AudioList>::dummy_result();
    });
}

void Au3AudioComService::clearAudioListCache()
{
    m_audioListCache.clear();
    m_audiosPerBatch = 0;
}
