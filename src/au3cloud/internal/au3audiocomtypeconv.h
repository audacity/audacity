/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "framework/global/io/path.h"

#include "au3-cloud-audiocom/sync/CloudSyncDTO.h"
#include "au3-cloud-audiocom/sync/CloudSyncError.h"
#include "au3-cloud-audiocom/UploadService.h"
#include "au3-cloud-audiocom/NetworkUtils.h"

#include "au3cloud/cloudtypes.h"
#include "au3cloud/au3clouderrors.h"

namespace au::au3cloud {
ProjectList convertFromAu3PaginatedProject(const audacity::cloud::audiocom::sync::PaginatedProjectsResponse& paginatedResponse);

AudioList convertFromAu3CloudAudio(const audacity::cloud::audiocom::sync::PaginatedAudioResponse& paginatedResponse,
                                   const muse::io::path_t& thumbnailCacheDir);

Err cloudSyncErrorToErr(const std::optional<audacity::cloud::audiocom::sync::CloudSyncError>& error);

Err uploadResultToErr(audacity::cloud::audiocom::UploadOperationCompleted::Result result);

Err syncResultCodeToErr(audacity::cloud::audiocom::SyncResultCode code);

std::vector<DownloadRequest> convertToDownloadRequests(const audacity::cloud::audiocom::sync::PaginatedAudioResponse& paginatedResponse,
                                                       const muse::io::path_t& thumbnailCacheDir);
}
