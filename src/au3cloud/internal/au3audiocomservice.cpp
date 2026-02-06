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

muse::async::Promise<ProjectList> Au3AudioComService::downloadProjectList(size_t scoresPerBatch, size_t batchNumber)
{
    return muse::async::Promise<ProjectList>([scoresPerBatch, batchNumber](auto resolve, auto) {
        auto& cloudSyncService = CloudSyncService::Get();
        auto cancellationContext = CancellationContext::Create();
        auto future = cloudSyncService.GetProjects(cancellationContext, 1, 20, "");
        auto result = future.get();

        const auto* paginatedResponse = std::get_if<sync::PaginatedProjectsResponse>(&result);
        if (paginatedResponse) {
            const auto projects = convertFromAu3PaginatedProject(*paginatedResponse);
            (void)resolve(projects);
        } else {
            ProjectList projects;
            (void)resolve(projects);
        }

        return muse::async::Promise<ProjectList>::dummy_result();
    });
}

muse::async::Promise<AudioList> Au3AudioComService::downloadAudioList(size_t audiosPerBatch, size_t batchNumber)
{
    return muse::async::Promise<AudioList>([audiosPerBatch, batchNumber](auto resolve, auto) {
        auto& cloudSyncService = CloudSyncService::Get();
        auto cancellationContext = CancellationContext::Create();
        auto future = cloudSyncService.GetAudioList(cancellationContext, audiosPerBatch, batchNumber, "");
        auto result = future.get();

        const auto* paginatedResponse = std::get_if<sync::PaginatedAudioResponse>(&result);
        if (paginatedResponse) {
            AudioList audioList = convertFromAu3CloudAudio(*paginatedResponse);
            (void)resolve(audioList);
        } else {
            AudioList audioList;
            (void)resolve(audioList);
        }

        return muse::async::Promise<AudioList>::dummy_result();
    });
}
