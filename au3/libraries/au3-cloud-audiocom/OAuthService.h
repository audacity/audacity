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
#include <string>
#include <mutex>

#include "Observer.h"

enum class AudiocomTrace;

namespace audacity::cloud::audiocom {
class ServiceConfig;

//! Message that is sent when authorization state changes.
struct CLOUD_AUDIOCOM_API AuthStateChangedMessage final
{
    //! OAuth access token, valid for the current session or less.
    std::string_view accessToken;
    //! Error message returned by the server in case of oauth error.
    std::string_view errorMessage;
    AudiocomTrace trace;
    //! Flag that indicates if user is authorised.
    bool authorised;
    bool silent;
};

//! Service responsible for OAuth authentication against the audio.com service.
class CLOUD_AUDIOCOM_API OAuthService final : public Observer::Publisher<AuthStateChangedMessage>
{
public:
    using AuthSuccessCallback = std::function<void (std::string_view)>;
    using AuthFailureCallback = std::function<void (unsigned, std::string_view)>;

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
       \p silent indicates, that the service should not attempt to show any UI.

       Callback argument will contain an access token, if any, or an empty view.
    */
    void ValidateAuth(
        AuthSuccessCallback completedHandler, AudiocomTrace, bool silent);

    //! Handle the OAuth callback
    /*
       This method supports the following URLs:
       - audacity://link?authorization_code=code
       - audacity://link?token=refresh_token
       - audacity://link?username=user&password=password

       This method will attempt to perform OAuth2 authorization and invoke \p completedHandler (if valid).
       Callback is potentially called from the network thread.

       \returns true if the URL was handled, false otherwise.
    */
    bool HandleLinkURI(
        std::string_view uri, AudiocomTrace, AuthSuccessCallback completedHandler);

    //! Removes access and refresh token, notifies about the logout.
    void UnlinkAccount(AudiocomTrace);

    //! Return the current access token, if any.
    std::string GetAccessToken() const;

    //! Creates a link to authorization request dialog
    // with selected OAuth provider
    static std::string MakeOAuthRequestURL(std::string_view authClientId);

    //! Creates a link to authorize audio.com using current auth token
    std::string MakeAudioComAuthorizeURL(std::string_view userId, std::string_view redirectUrl);

    void Authorize(std::string_view email, std::string_view password, AuthSuccessCallback successCallback,
                   AuthFailureCallback failureCallback, AudiocomTrace trace);

    //! Register a new user with \p email and \p password
    void Register(std::string_view email, std::string_view password, AuthSuccessCallback successCallback,
                  AuthFailureCallback failureCallback, AudiocomTrace trace);

private:
    void AuthorisePassword(
        const ServiceConfig& config, std::string_view userName, std::string_view password, AudiocomTrace,
        std::function<void(std::string_view)> completedHandler);

    void AuthoriseRefreshToken(
        const ServiceConfig& config, std::string_view refreshToken, AudiocomTrace, std::function<void(std::string_view)> completedHandler,
        bool silent);

    void AuthoriseRefreshToken(
        const ServiceConfig& config, AudiocomTrace, std::function<void(std::string_view)> completedHandler, bool silent);

    void AuthoriseCode(
        const ServiceConfig& config, std::string_view authorizationCode, bool useAudioComRedirectURI, AudiocomTrace,
        std::function<void(std::string_view)> completedHandler);

    void DoAuthorise(
        const ServiceConfig& config, std::string_view payload, AudiocomTrace, std::function<void(std::string_view)> completedHandler,
        bool silent);

    void ParseTokenResponse(std::string_view response, AuthSuccessCallback successCallback, AuthFailureCallback failureCallback,
                            AudiocomTrace trace, bool silent);

    void SafePublish(const AuthStateChangedMessage& message);

    using Clock = std::chrono::steady_clock;

    mutable std::recursive_mutex mMutex;

    Clock::time_point mTokenExpirationTime;
    std::string mAccessToken;
};

//! Returns the instance of the OAuthService
CLOUD_AUDIOCOM_API OAuthService& GetOAuthService();
} // namespace audacity::cloud::audiocom
