/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSyncError.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <string>

#include "NetworkUtils.h"

class TranslatableString;

namespace audacity::network_manager {
class IResponse;
} // namespace audacity::network_manager

namespace audacity::cloud::audiocom::sync {
struct CLOUD_AUDIOCOM_API CloudSyncError final
{
    enum ErrorType
    {
        None,
        Authorization,
        ProjectLimitReached,
        ProjectStorageLimitReached,
        ProjectVersionConflict,
        ProjectNotFound,
        DataUploadFailed,
        Network,
        Server,
        Cancelled,
        Aborted,
        ClientFailure,
    };

    ErrorType Type { };
    std::string ErrorMessage;
};

CLOUD_AUDIOCOM_API
CloudSyncError DeduceUploadError(audacity::network_manager::IResponse& response);

CLOUD_AUDIOCOM_API
CloudSyncError MakeClientFailure(const TranslatableString& message);

CLOUD_AUDIOCOM_API
CloudSyncError MakeClientFailure(const std::string& message);

CLOUD_AUDIOCOM_API
CloudSyncError MakeClientFailure(const char* message);

CLOUD_AUDIOCOM_API CloudSyncError::ErrorType
DeduceError(SyncResultCode code);
} // namespace audacity::cloud::audiocom::sync
