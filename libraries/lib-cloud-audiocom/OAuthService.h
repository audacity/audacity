/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  OAuthService.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <chrono>
#include <functional>
#include <string_view>
#include <mutex>

#include "Observer.h"

namespace cloud::audiocom
{
class ServiceConfig;

//! Message that is sent when authorization state changes.
struct CLOUD_AUDIOCOM_API AuthStateChangedMessage final
{
   //! OAuth access token, valid for the current session or less.
   std::string_view accessToken;
   //! Error message returned by the server in case of oauth error.
   std::string_view errorMessage;
   //! Flag that indicates if user is authorised.
   bool authorised;
};

//! Service responsible for OAuth authentication against the audio.com service.
class CLOUD_AUDIOCOM_API OAuthService final :
    public Observer::Publisher<AuthStateChangedMessage>
{
public:
   //! Indicates, that service has a valid access token, i. e. that the user is authorized.
   bool HasAccessToken() const;
   //! Indicates, that the service has a possibly valid refresh token, which can be used
   //! to authorize the user
   bool HasRefreshToken() const;

   //! Attempt to authorize the user.
   /*
      If `HasAccessToken() || !HasRefreshToken()` invokes \p completedHandler synchronously,
      if valid.

      Otherwise, the service will attempt to authorize the user and invoke
      \p completedHandler (if valid). Callback is invoked from the network thread.

      Callback argument will contain an access token, if any, or an empty view.
   */
   void ValidateAuth(std::function<void(std::string_view)> completedHandler);

   //! Handle the OAuth callback
   /*
      This method supports the following URLs:
      - audacity://link?authorization_code=code
      - audacity://link?token=refresh_token
      - audacity://link?username=user&password=password

      This method will attempt to perform OAuth2 authorization and invoke \p completedHandler (if valid).
      Callback is potentially called from the network thread.
   */
   void HandleLinkURI(
      std::string_view uri,
      std::function<void(std::string_view)> completedHandler);

   //! Removes access and refresh token, notifies about the logout.
   void UnlinkAccount();

   //! Return the current access token, if any.
   std::string GetAccessToken() const;

private:
   void AuthorisePassword(
      const ServiceConfig& config, std::string_view userName,
      std::string_view password,
      std::function<void(std::string_view)> completedHandler);

   void AuthoriseRefreshToken(
      const ServiceConfig& config, std::string_view refreshToken,
      std::function<void(std::string_view)> completedHandler);

   void AuthoriseRefreshToken(
      const ServiceConfig& config,
      std::function<void(std::string_view)> completedHandler);

   void AuthoriseCode(
      const ServiceConfig& config, std::string_view authorizationCode,
      std::function<void(std::string_view)> completedHandler);

   void DoAuthorise(
      const ServiceConfig& config, std::string_view payload,
      std::function<void(std::string_view)> completedHandler);

   void SafePublish(const AuthStateChangedMessage& message);

   using Clock = std::chrono::steady_clock;

   mutable std::recursive_mutex mMutex;

   Clock::time_point mTokenExpirationTime;
   std::string mAccessToken;
};


//! Returns the instance of the OAuthService
CLOUD_AUDIOCOM_API OAuthService& GetOAuthService();

} // namespace cloud::audiocom
