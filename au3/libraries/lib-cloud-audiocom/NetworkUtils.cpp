/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  NetworkUtils.cpp

  Dmitry Vedenko

**********************************************************************/

#include "NetworkUtils.h"

#include <algorithm>

#include "OAuthService.h"
#include "ServiceConfig.h"

#include "IResponse.h"
#include "Request.h"

#include "TranslatableString.h"

namespace audacity::cloud::audiocom {
using namespace audacity::network_manager;

namespace {
SyncResultCode GetResultCodeFromHttpCode(int code) noexcept
{
    if (code == HttpCode::Conflict || code == HttpCode::UnprocessableEntity) {
        return SyncResultCode::Conflict;
    }

    if (code == HttpCode::PaymentRequired) {
        return SyncResultCode::PaymentRequired;
    }

    if (code == HttpCode::Unauthorized) {
        return SyncResultCode::Unauthorized;
    }

    if (code == HttpCode::Forbidden) {
        return SyncResultCode::Forbidden;
    }

    if (code == HttpCode::NotFound) {
        return SyncResultCode::NotFound;
    }

    if (code == HttpCode::PayloadTooLarge) {
        return SyncResultCode::TooLarge;
    }

    if (code == HttpCode::Gone) {
        return SyncResultCode::Expired;
    }

    if (code > 500) {
        return SyncResultCode::InternalServerError;
    }

    return SyncResultCode::UnknownError;
}

SyncResultCode GuessResultCode(IResponse& response) noexcept
{
    const auto code = response.getError();

    if (code == NetworkError::NoError) {
        return SyncResultCode::Success;
    }

    if (code == NetworkError::OperationCancelled) {
        return SyncResultCode::Cancelled;
    }

    if (
        code == NetworkError::ConnectionFailed
        || code == NetworkError::ConnectionRefused
        || code == NetworkError::HostNotFound
        || code == NetworkError::ProxyConnectionFailed
        || code == NetworkError::ProxyNotFound
        || code == NetworkError::RemoteHostClosed
        || code == NetworkError::Timeout) {
        return SyncResultCode::ConnectionFailed;
    }

    if (code == NetworkError::HTTPError) {
        return GetResultCodeFromHttpCode(response.getHTTPCode());
    }

    return SyncResultCode::UnknownError;
}
} // namespace

ResponseResult GetResponseResult(IResponse& response, bool readBody)
{
    const auto resultCode = GuessResultCode(response);

    if (resultCode == SyncResultCode::Success) {
        return { resultCode,
                 readBody ? response.readAll<std::string>() : std::string {} }
    }

    if (response.getError() != NetworkError::HTTPError) {
        return { resultCode, response.getErrorString() }
    }

    return { resultCode,
             std::string("HTTP ") + std::to_string(response.getHTTPCode())
             + std::string("\n") + response.readAll<std::string>() };
}

void SetCommonHeaders(Request& request)
{
    const auto language = GetServiceConfig().GetAcceptLanguageValue();

    if (!language.empty()) {
        request.setHeader(
            audacity::network_manager::common_headers::AcceptLanguage, language);
    }

    auto& oauthService = GetOAuthService();

    if (oauthService.HasAccessToken()) {
        request.setHeader(
            common_headers::Authorization, oauthService.GetAccessToken());
    }
}

bool IsUploadRecoverable(SyncResultCode code)
{
    return code == SyncResultCode::Cancelled
           || code == SyncResultCode::ConnectionFailed
           || code == SyncResultCode::PaymentRequired
           || code == SyncResultCode::Unauthorized
           || code == SyncResultCode::Forbidden
           || code == SyncResultCode::InternalClientError
           || code == SyncResultCode::InternalServerError;
}

TransferStats& TransferStats::SetBytesTransferred(int64_t bytesTransferred)
{
    BytesTransferred = bytesTransferred;
    return *this;
}

TransferStats& TransferStats::SetBlocksTransferred(int64_t blocksTransferred)
{
    BlocksTransferred = blocksTransferred;
    return *this;
}

TransferStats&
TransferStats::SetProjectFilesTransferred(int64_t projectFilesTransferred)
{
    ProjectFilesTransferred = projectFilesTransferred;
    return *this;
}

TransferStats& TransferStats::SetTransferDuration(Duration transferDuration)
{
    TransferDuration = transferDuration;
    return *this;
}
} // namespace audacity::cloud::audiocom
