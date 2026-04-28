/*
* Audacity: A Digital Audio Editor
*/
#include "au3audiocomtypeconv.h"

namespace au::au3cloud {
ProjectList convertFromAu3PaginatedProject(const audacity::cloud::audiocom::sync::PaginatedProjectsResponse& paginatedResponse)
{
    ProjectList projectList;

    projectList.items.reserve(paginatedResponse.Items.size());
    for (size_t i = 0; i < paginatedResponse.Items.size(); i++) {
        const auto& projectInfo = paginatedResponse.Items[i];

        ProjectList::Item item;
        item.id = projectInfo.Id;
        item.name = projectInfo.Name;
        item.slug = projectInfo.Slug;
        item.authorName = projectInfo.AuthorName;
        item.username = projectInfo.Username;
        item.details = projectInfo.Details;
        item.lastSyncedSnapshotId = projectInfo.LastSyncedSnapshotId;
        item.created = projectInfo.Created;
        item.updated = projectInfo.Updated;
        item.fileSize = projectInfo.Size;
        item.headSnapshotSynced = projectInfo.HeadSnapshot.Synced;

        projectList.items.push_back(std::move(item));
    }

    projectList.meta.total = paginatedResponse.Pagination.TotalCount;
    projectList.meta.batches = paginatedResponse.Pagination.PagesCount;
    projectList.meta.thisBatchNumber = paginatedResponse.Pagination.CurrentPage;
    projectList.meta.itemsPerBatch = paginatedResponse.Pagination.PageSize;

    return projectList;
}

AudioList convertFromAu3CloudAudio(const audacity::cloud::audiocom::sync::PaginatedAudioResponse& paginatedResponse,
                                   const muse::io::path_t& thumbnailCacheDir)
{
    AudioList audioList;

    audioList.items.reserve(paginatedResponse.Items.size());
    for (size_t i = 0; i < paginatedResponse.Items.size(); i++) {
        const auto& audioInfo = paginatedResponse.Items[i];

        AudioList::Item item;
        item.id = audioInfo.Id;
        item.username = audioInfo.Username;
        item.authorName = audioInfo.AuthorName;
        item.slug = audioInfo.Slug;
        item.title = audioInfo.Title;
        item.tags = audioInfo.Tags;
        item.created = audioInfo.Created;
        item.fileSize = audioInfo.FileSize;
        item.duration = audioInfo.Duration;
        item.waveformPath = audioInfo.Id.empty() ? muse::io::path_t() : thumbnailCacheDir.appendingComponent(audioInfo.Id).appendingSuffix(
            "json");

        audioList.items.push_back(std::move(item));
    }

    audioList.meta.total = paginatedResponse.Pagination.TotalCount;
    audioList.meta.batches = paginatedResponse.Pagination.PagesCount;
    audioList.meta.thisBatchNumber = paginatedResponse.Pagination.CurrentPage;
    audioList.meta.itemsPerBatch = paginatedResponse.Pagination.PageSize;

    return audioList;
}

Err cloudSyncErrorToErr(const std::optional<audacity::cloud::audiocom::sync::CloudSyncError>& error)
{
    if (!error.has_value()) {
        return Err::UnknownError;
    }

    using ErrorType = audacity::cloud::audiocom::sync::CloudSyncError::ErrorType;
    switch (error->Type) {
    case ErrorType::None:
        return Err::NoError;
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

Err uploadResultToErr(audacity::cloud::audiocom::UploadOperationCompleted::Result result)
{
    using Result = audacity::cloud::audiocom::UploadOperationCompleted::Result;
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

Err syncResultCodeToErr(audacity::cloud::audiocom::SyncResultCode code)
{
    switch (code) {
    case audacity::cloud::audiocom::SyncResultCode::Success:
        return Err::NoError;
    case audacity::cloud::audiocom::SyncResultCode::Cancelled:
        return Err::SyncResultCancelled;
    case audacity::cloud::audiocom::SyncResultCode::Expired:
        return Err::SyncResultExpired;
    case audacity::cloud::audiocom::SyncResultCode::Conflict:
        return Err::SyncResultConflict;
    case audacity::cloud::audiocom::SyncResultCode::ConnectionFailed:
        return Err::SyncResultConnectionFailed;
    case audacity::cloud::audiocom::SyncResultCode::PaymentRequired:
        return Err::SyncResultPaymentRequired;
    case audacity::cloud::audiocom::SyncResultCode::TooLarge:
        return Err::SyncResultTooLarge;
    case audacity::cloud::audiocom::SyncResultCode::Unauthorized:
        return Err::SyncResultUnauthorized;
    case audacity::cloud::audiocom::SyncResultCode::Forbidden:
        return Err::SyncResultForbidden;
    case audacity::cloud::audiocom::SyncResultCode::NotFound:
        return Err::SyncResultNotFound;
    case audacity::cloud::audiocom::SyncResultCode::UnexpectedResponse:
        return Err::SyncResultUnexpectedResponse;
    case audacity::cloud::audiocom::SyncResultCode::InternalClientError:
        return Err::SyncResultInternalClientError;
    case audacity::cloud::audiocom::SyncResultCode::InternalServerError:
        return Err::SyncResultInternalServerError;
    case audacity::cloud::audiocom::SyncResultCode::SyncImpossible:
        return Err::SyncResultSyncImpossible;
    case audacity::cloud::audiocom::SyncResultCode::UnknownError:
        return Err::UnknownError;
    }

    return Err::UnknownError;
}

std::vector<DownloadRequest> convertToDownloadRequests(const audacity::cloud::audiocom::sync::PaginatedAudioResponse& paginatedResponse,
                                                       const muse::io::path_t& thumbnailCacheDir)
{
    std::vector<DownloadRequest> requests;

    requests.reserve(paginatedResponse.Items.size());
    for (size_t i = 0; i < paginatedResponse.Items.size(); i++) {
        const auto& audioInfo = paginatedResponse.Items[i];

        if (audioInfo.Id.empty()) {
            continue;
        }

        DownloadRequest request;
        request.id = audioInfo.Id;
        request.url = audioInfo.WaveformUrl;
        request.localPath = thumbnailCacheDir.appendingComponent(audioInfo.Id).appendingSuffix("json");

        requests.push_back(std::move(request));
    }

    return requests;
}
}
