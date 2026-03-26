/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "framework/global/types/ret.h"

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

    // ProjectSyncResult::StatusCode
    SyncResultSuccess,
    SyncResultCancelled,
    SyncResultExpired,
    SyncResultConflict,
    SyncResultConnectionFailed,
    SyncResultPaymentRequired,
    SyncResultTooLarge,
    SyncResultUnauthorized,
    SyncResultForbidden,
    SyncResultNotFound,
    SyncResultUnexpectedResponse,
    SyncResultInternalClientError,
    SyncResultInternalServerError,
    SyncResultSyncImpossible,
    SyncResultUnknownError,
};

inline muse::Ret make_ret(Err e)
{
    int retCode = static_cast<int>(e);
    return muse::Ret(retCode);
}
}
