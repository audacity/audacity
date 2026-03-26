/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "framework/global/types/ret.h"
#include "framework/global/translation.h"

namespace au::au3cloud {
static constexpr int AU3_CLOUD_FIRST = 6000;

enum class Err {
    Undefined    = int(muse::Ret::Code::Undefined),
    NoError      = int(muse::Ret::Code::Ok),
    UnknownError = AU3_CLOUD_FIRST,

    NoExportPlugin,
    NoExtensions,

    // shareAudio — UploadOperationCompleted::Result
    UploadAborted,
    UploadFileNotFound,
    UploadUnauthorized,
    UploadInvalidData,
    UploadUnexpectedResponse,
    UploadFailed,

    // uploadProject — CloudSyncError::ErrorType
    AuthorizationRequired,
    ProjectLimitReached,
    ProjectStorageLimitReached,
    ProjectVersionConflict,
    ProjectNotFound,
    DataUploadFailed,
    NetworkError,
    ServerError,
    SyncCancelled,
    SyncAborted,
    ClientFailure,
    SnapshotFailed,
};

inline muse::Ret make_ret(Err e)
{
    int retCode = static_cast<int>(e);

    switch (e) {
    case Err::Undefined:                  return muse::Ret(retCode);
    case Err::NoError:                    return muse::Ret(retCode);
    case Err::UnknownError:               return muse::Ret(retCode);

    case Err::NoExportPlugin:             return muse::Ret(retCode, muse::trc("cloud", "No audio export plugin available"));
    case Err::NoExtensions:               return muse::Ret(retCode, muse::trc("cloud", "No file extensions available for audio format"));

    case Err::UploadAborted:              return muse::Ret(retCode, muse::trc("cloud", "Upload was aborted"));
    case Err::UploadFileNotFound:         return muse::Ret(retCode, muse::trc("cloud", "Exported audio file not found before upload"));
    case Err::UploadUnauthorized:         return muse::Ret(retCode, muse::trc("cloud", "Authorization required to upload audio"));
    case Err::UploadInvalidData:          return muse::Ret(retCode, muse::trc("cloud", "Invalid data was sent to audio.com"));
    case Err::UploadUnexpectedResponse:   return muse::Ret(retCode, muse::trc("cloud", "Unexpected response from audio.com"));
    case Err::UploadFailed:              return muse::Ret(retCode, muse::trc("cloud", "Audio upload failed"));

    case Err::AuthorizationRequired:      return muse::Ret(retCode, muse::trc("cloud", "Authorization required"));
    case Err::ProjectLimitReached:        return muse::Ret(retCode, muse::trc("cloud", "Project limit reached"));
    case Err::ProjectStorageLimitReached: return muse::Ret(retCode, muse::trc("cloud", "Project storage limit reached"));
    case Err::ProjectVersionConflict:     return muse::Ret(retCode, muse::trc("cloud", "Project version conflict"));
    case Err::ProjectNotFound:            return muse::Ret(retCode, muse::trc("cloud", "Project not found on audio.com"));
    case Err::DataUploadFailed:           return muse::Ret(retCode, muse::trc("cloud", "Data upload failed"));
    case Err::NetworkError:               return muse::Ret(retCode, muse::trc("cloud", "Network error while syncing to audio.com"));
    case Err::ServerError:                return muse::Ret(retCode, muse::trc("cloud", "Server error from audio.com"));
    case Err::SyncCancelled:              return muse::Ret(retCode, muse::trc("cloud", "Project sync was cancelled"));
    case Err::SyncAborted:                return muse::Ret(retCode, muse::trc("cloud", "Project sync was aborted"));
    case Err::ClientFailure:              return muse::Ret(retCode, muse::trc("cloud", "Client failure while syncing to audio.com"));
    case Err::SnapshotFailed:             return muse::Ret(retCode, muse::trc("cloud", "Failed to create project snapshot for upload"));
    }

    return muse::Ret(retCode);
}
} // namespace au::au3cloud
