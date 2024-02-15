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

namespace cloud::audiocom
{
using namespace audacity::network_manager;

namespace
{
ResponseResultCode GetResultCodeFromHttpCode(int code) noexcept
{
   if (code == HttpCode::Conflict || code == HttpCode::UnprocessableEntity)
      return ResponseResultCode::Conflict;

   if (code == HttpCode::PaymentRequired)
      return ResponseResultCode::PaymentRequired;

   if (code == HttpCode::Unauthorized)
      return ResponseResultCode::Unauthorized;

   if (code == HttpCode::Forbidden)
      return ResponseResultCode::Forbidden;

   if (code == HttpCode::NotFound)
      return ResponseResultCode::NotFound;

   if (code == HttpCode::PayloadTooLarge)
      return ResponseResultCode::TooLarge;

   return ResponseResultCode::Expired;
}

ResponseResultCode GuessResultCode(IResponse& response) noexcept
{
   const auto code = response.getError();

   if (code == NetworkError::NoError)
      return ResponseResultCode::Success;

   if (code == NetworkError::OperationCancelled)
      return ResponseResultCode::Cancelled;

   if (
      code == NetworkError::ConnectionFailed ||
      code == NetworkError::ConnectionRefused ||
      code == NetworkError::HostNotFound ||
      code == NetworkError::ProxyConnectionFailed ||
      code == NetworkError::ProxyNotFound)
      return ResponseResultCode::ConnectionFailed;

   if (code == NetworkError::HTTPError)
      return GetResultCodeFromHttpCode(response.getHTTPCode());

   return ResponseResultCode::UnknownError;
}
} // namespace

ResponseResult GetResponseResult(IResponse& response, bool readBody)
{
   const auto resultCode = GuessResultCode(response);

   if (resultCode == ResponseResultCode::Success)
      return { resultCode,
               readBody ? response.readAll<std::string>() : std::string {} };

   if (response.getError() != NetworkError::HTTPError)
      return { resultCode, response.getErrorString() };

   return { resultCode,
            std::string("HTTP ") + std::to_string(response.getHTTPCode()) +
               std::string("\n") + response.readAll<std::string>() };
}

void SetCommonHeaders(Request& request)
{
   const auto language = GetServiceConfig().GetAcceptLanguageValue();

   if (!language.empty())
      request.setHeader(
         audacity::network_manager::common_headers::AcceptLanguage, language);

   auto& oauthService = GetOAuthService();

   if (oauthService.HasAccessToken())
      request.setHeader(
         common_headers::Authorization, oauthService.GetAccessToken());
}

CancellationContext::CancellationContext(Tag)
{
}

std::shared_ptr<CancellationContext> CancellationContext::Create()
{
   return std::make_shared<CancellationContext>(Tag {});
}

void CancellationContext::Cancel()
{
   if (mCancelled.exchange(true))
      return;

   std::vector<std::function<void()>> callbacks;

   {
      auto lock = std::lock_guard { mDeferredCallbacksMutex };
      std::swap(callbacks, mDeferredCallbacks);
   }

   std::for_each(
      callbacks.begin(), callbacks.end(), [](auto& callback) { callback(); });
}

void CancellationContext::OnCancelled(std::function<void()> callback)
{
   if (!callback)
      return;

   if (mCancelled.load())
   {
      callback();
      return;
   }

   auto lock = std::lock_guard { mDeferredCallbacksMutex };
   mDeferredCallbacks.push_back(std::move(callback));
}

void CancellationContext::OnCancelled(
   std::shared_ptr<audacity::network_manager::IResponse> response)
{
   if (!response)
      return;

   OnCancelled([response = std::move(response)]() { response->abort(); });
}

} // namespace cloud::audiocom
