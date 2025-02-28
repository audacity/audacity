/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  OAuthService.cpp

  Dmitry Vedenko

**********************************************************************/

#include "OAuthService.h"

#include <cassert>
#include <cctype>

#include <rapidjson/document.h>
#include <rapidjson/writer.h>

#include "CodeConversions.h"
#include "Prefs.h"

#include "IResponse.h"
#include "NetworkManager.h"
#include "Request.h"

#include "ServiceConfig.h"

#include "UrlDecode.h"

#include "BasicUI.h"
#include "ExportUtils.h"

#include "StringUtils.h"
#include "UrlEncode.h"

namespace audacity::cloud::audiocom {
namespace {
StringSetting refreshToken { L"/cloud/audiocom/refreshToken", "" };

const std::string_view uriPrefix = "audacity://link";
const std::string_view usernamePrefix = "username=";
const std::string_view passwordPrefix = "password=";
const std::string_view tokenPrefix = "token=";
const std::string_view authClientPrefix = "authclient=";
const std::string_view responseTypePrefix = "response_type=";
const std::string_view clientIdPrefix = "client_id=";
const std::string_view authorizationCodePrefix = "authorization_code=";
const std::string_view codePrefix = "code=";
const std::string_view urlPrefix = "url=";
const std::string_view userPrefix = "user=";

void WriteClientFields(rapidjson::Document& document)
{
    using namespace rapidjson;

    const auto clientID = GetServiceConfig().GetOAuthClientID();
    const auto clientSecret = GetServiceConfig().GetOAuthClientSecret();

    document.AddMember(
        "client_id",
        Value(clientID.data(), clientID.size(), document.GetAllocator()),
        document.GetAllocator());

    document.AddMember(
        "client_secret",
        Value(clientSecret.data(), clientSecret.size(), document.GetAllocator()),
        document.GetAllocator());
}

void WriteAccessFields(rapidjson::Document& document, std::string_view grantType, std::string_view scope)
{
    using namespace rapidjson;

    document.AddMember(
        "grant_type", StringRef(grantType.data(), grantType.size()),
        document.GetAllocator());

    document.AddMember(
        "scope", StringRef(scope.data(), scope.size()), document.GetAllocator());
}

void WriteCommonFields(
    rapidjson::Document& document, std::string_view grantType, std::string_view scope)
{
    WriteClientFields(document);
    WriteAccessFields(document, grantType, scope);
}

template<typename Elem, typename First, typename ...Others>
void append(std::basic_string<Elem>& dest, First&& first, Others&& ... others)
{
    dest.append(first);
    if constexpr (sizeof...(others) != 0) {
        append(dest, std::forward<Others>(others)...);
    }
}

template<typename First, typename ...Others>
auto concat(First&& first, Others&& ... others)
{
    std::basic_string<typename First::value_type> dest(first);
    append(dest, std::forward<Others>(others)...);
    return dest;
}
} // namespace

void OAuthService::ValidateAuth(
    std::function<void(std::string_view)> completedHandler, AudiocomTrace trace,
    bool silent)
{
    if (HasAccessToken() || !HasRefreshToken()) {
        if (completedHandler) {
            completedHandler(GetAccessToken());
        }
        return;
    }

    AuthoriseRefreshToken(
        GetServiceConfig(), trace, std::move(completedHandler), silent);
}

bool OAuthService::HandleLinkURI(
    std::string_view uri, AudiocomTrace trace,
    std::function<void(std::string_view)> completedHandler)
{
    if (!IsPrefixedInsensitive(uri, uriPrefix)) {
        if (completedHandler) {
            completedHandler({});
        }
        return false;
    }

    // It was observed, that sometimes link is passed as audacity://link/
    // This is valid trace URI point of view, but we need to handle it separately
    const auto argsStart = uri.find("?");

    if (argsStart == std::string_view::npos) {
        if (completedHandler) {
            completedHandler({});
        }
        return false;
    }

    // Length is handled in IsPrefixed
    auto args = uri.substr(argsStart + 1);

    std::string_view token;
    std::string_view username;
    std::string_view password;
    std::string_view authorizationCode;
    auto useAudioComRedirectURI = false;

    while (!args.empty())
    {
        const auto nextArg = args.find('&');

        const auto arg = args.substr(0, nextArg);
        args = nextArg == std::string_view::npos ? "" : args.substr(nextArg + 1);

        if (IsPrefixed(arg, usernamePrefix)) {
            username = arg.substr(usernamePrefix.length());
        } else if (IsPrefixed(arg, passwordPrefix)) {
            password = arg.substr(passwordPrefix.length());
        } else if (IsPrefixed(arg, tokenPrefix)) {
            token = arg.substr(tokenPrefix.length());
        } else if (IsPrefixed(arg, authorizationCodePrefix)) {
            //authorization code was generated for audio.com, not audacity...
            useAudioComRedirectURI = true;
            authorizationCode = arg.substr(authorizationCodePrefix.length());
        } else if (IsPrefixed(arg, codePrefix)) {
            authorizationCode = arg.substr(codePrefix.length());
        }
    }

    // Some browsers (safari) add an extra trailing chars we don't need
    size_t hashPos = authorizationCode.find('#');

    if (hashPos != std::string::npos) {
        authorizationCode = authorizationCode.substr(0, hashPos);
    }

    // We have a prioritized list of authorization methods
    if (!authorizationCode.empty()) {
        AuthoriseCode(
            GetServiceConfig(), authorizationCode, useAudioComRedirectURI, trace,
            std::move(completedHandler));
    } else if (!token.empty()) {
        AuthoriseRefreshToken(
            GetServiceConfig(), token, trace, std::move(completedHandler), false);
    } else if (!username.empty() && !password.empty()) {
        AuthorisePassword(
            GetServiceConfig(), audacity::UrlDecode(std::string(username)),
            audacity::UrlDecode(std::string(password)), trace,
            std::move(completedHandler));
    } else {
        if (completedHandler) {
            completedHandler({});
        }

        return false;
    }

    return true;
}

void OAuthService::UnlinkAccount(AudiocomTrace trace)
{
    std::lock_guard<std::recursive_mutex> lock(mMutex);

    mAccessToken.clear();
    refreshToken.Write({});
    gPrefs->Flush();

    // Unlink account is expected to be called only
    // on UI thread
    Publish({ {}, {}, trace, false });
}

void OAuthService::AuthorisePassword(
    const ServiceConfig& config, std::string_view userName,
    std::string_view password, AudiocomTrace trace,
    std::function<void(std::string_view)> completedHandler)
{
    using namespace rapidjson;

    Document document;
    document.SetObject();

    WriteCommonFields(document, "password", "all");

    document.AddMember(
        "username", StringRef(userName.data(), userName.size()),
        document.GetAllocator());

    document.AddMember(
        "password", StringRef(password.data(), password.size()),
        document.GetAllocator());

    rapidjson::StringBuffer buffer;
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
    document.Accept(writer);

    DoAuthorise(
        config, { buffer.GetString(), buffer.GetSize() }, trace,
        std::move(completedHandler), false);
}

void OAuthService::AuthoriseRefreshToken(
    const ServiceConfig& config, std::string_view token, AudiocomTrace trace,
    std::function<void(std::string_view)> completedHandler, bool silent)
{
    using namespace rapidjson;

    Document document;
    document.SetObject();

    WriteCommonFields(document, "refresh_token", "");

    document.AddMember(
        "refresh_token", StringRef(token.data(), token.size()),
        document.GetAllocator());

    rapidjson::StringBuffer buffer;
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
    document.Accept(writer);

    DoAuthorise(
        config, { buffer.GetString(), buffer.GetSize() }, trace,
        std::move(completedHandler), silent);
}

void OAuthService::AuthoriseRefreshToken(
    const ServiceConfig& config, AudiocomTrace trace,
    std::function<void(std::string_view)> completedHandler, bool silent)
{
    std::lock_guard<std::recursive_mutex> lock(mMutex);

    AuthoriseRefreshToken(
        config, audacity::ToUTF8(refreshToken.Read()), trace,
        std::move(completedHandler), silent);
}

void OAuthService::AuthoriseCode(
    const ServiceConfig& config, std::string_view authorizationCode, bool useAudioComRedirectURI,
    AudiocomTrace trace, std::function<void(std::string_view)> completedHandler)
{
    using namespace rapidjson;

    Document document;
    document.SetObject();

    WriteCommonFields(document, "authorization_code", "all");

    document.AddMember(
        "code", StringRef(authorizationCode.data(), authorizationCode.size()),
        document.GetAllocator());

    const auto redirectURI = useAudioComRedirectURI ? config.GetOAuthRedirectURL() : std::string("audacity://link");

    document.AddMember(
        "redirect_uri", StringRef(redirectURI.data(), redirectURI.size()),
        document.GetAllocator());

    rapidjson::StringBuffer buffer;
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
    document.Accept(writer);

    DoAuthorise(
        config, { buffer.GetString(), buffer.GetSize() }, trace,
        std::move(completedHandler), false);
}

bool OAuthService::HasAccessToken() const
{
    return !GetAccessToken().empty();
}

bool OAuthService::HasRefreshToken() const
{
    std::lock_guard<std::recursive_mutex> lock(mMutex);
    return !refreshToken.Read().empty();
}

std::string OAuthService::GetAccessToken() const
{
    std::lock_guard<std::recursive_mutex> lock(mMutex);

    if (Clock::now() < mTokenExpirationTime) {
        return mAccessToken;
    }

    return {};
}

std::string OAuthService::MakeOAuthRequestURL(std::string_view authClientId)
{
    using namespace audacity::network_manager;

    return concat(
        GetServiceConfig().GetAPIUrl("/auth/authorize?"),
        authClientPrefix, authClientId, "&",
        responseTypePrefix, "code", "&",
        clientIdPrefix, GetServiceConfig().GetOAuthClientID(),
        "&redirect_uri=audacity://link"
        );
}

std::string OAuthService::MakeAudioComAuthorizeURL(std::string_view userId, std::string_view redirectUrl)
{
    auto token = GetAccessToken();

    // Remove token type from the token string
    size_t pos = token.find(' ');
    if (pos != std::string::npos) {
        token = token.substr(pos + 1);
    }

    return concat(
        GetServiceConfig().GetAuthWithRedirectURL(), "?",
        tokenPrefix, token, "&",
        userPrefix, userId, "&",
        urlPrefix, redirectUrl
        );
}

void OAuthService::Authorize(std::string_view email,
                             std::string_view password,
                             AuthSuccessCallback onSuccess,
                             AuthFailureCallback onFailure,
                             AudiocomTrace trace)
{
    using namespace audacity::network_manager;
    using rapidjson::StringRef;

    rapidjson::Document document;
    document.SetObject();

    WriteCommonFields(document, "password", "all");

    document.AddMember(
        "username", StringRef(email.data(), email.size()),
        document.GetAllocator());

    document.AddMember(
        "password", StringRef(password.data(), password.size()),
        document.GetAllocator());

    rapidjson::StringBuffer buffer;
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
    document.Accept(writer);

    Request request(GetServiceConfig().GetAPIUrl("/auth/token"));
    request.setHeader(common_headers::ContentType, common_content_types::ApplicationJson);
    request.setHeader(common_headers::Accept, common_content_types::ApplicationJson);

    auto response = NetworkManager::GetInstance().doPost(request,  buffer.GetString(), buffer.GetSize());
    response->setRequestFinishedCallback(
        [response, this, trace,
         onSuccess = std::move(onSuccess),
         onFailure = std::move(onFailure)](auto) mutable
    {
        const auto httpCode = response->getHTTPCode();
        const auto body = response->readAll<std::string>();
        if (httpCode == 200) {
            ParseTokenResponse(body, std::move(onSuccess), std::move(onFailure), trace, false);
        } else {
            if (onFailure) {
                onFailure(httpCode, body);
            }

            SafePublish({ {}, body, trace, false, false });
        }
    });
}

void OAuthService::Register(std::string_view email,
                            std::string_view password,
                            AuthSuccessCallback successCallback,
                            AuthFailureCallback failureCallback,
                            AudiocomTrace trace)
{
    using namespace audacity::network_manager;
    using rapidjson::StringRef;

    rapidjson::Document document;
    document.SetObject();

    WriteClientFields(document);

    document.AddMember(
        "email", StringRef(email.data(), email.size()),
        document.GetAllocator());

    document.AddMember(
        "password", StringRef(password.data(), password.size()),
        document.GetAllocator());

    rapidjson::StringBuffer buffer;
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
    document.Accept(writer);

    Request request(GetServiceConfig().GetAPIUrl("/auth/register"));
    request.setHeader(common_headers::ContentType, common_content_types::ApplicationJson);
    request.setHeader(common_headers::Accept, common_content_types::ApplicationJson);

    auto response = NetworkManager::GetInstance().doPost(request,  buffer.GetString(), buffer.GetSize());
    response->setRequestFinishedCallback(
        [response, this, trace,
         successCallback = std::move(successCallback),
         failureCallback = std::move(failureCallback)] (auto) mutable
    {
        const auto httpCode = response->getHTTPCode();
        const auto body = response->readAll<std::string>();
        if (httpCode == 200) {
            ParseTokenResponse(body, std::move(successCallback), std::move(failureCallback), trace, false);
        } else {
            if (failureCallback) {
                failureCallback(httpCode, body);
            }

            SafePublish({ {}, body, trace, false, false });
        }
    });
}

void OAuthService::DoAuthorise(
    const ServiceConfig& config, std::string_view payload, AudiocomTrace trace,
    AuthSuccessCallback completedHandler, bool silent)
{
    using namespace audacity::network_manager;

    Request request(config.GetAPIUrl("/auth/token"));

    request.setHeader(
        common_headers::ContentType, common_content_types::ApplicationJson);

    request.setHeader(
        common_headers::Accept, common_content_types::ApplicationJson);

    auto response = NetworkManager::GetInstance().doPost(
        request, payload.data(), payload.size());

    response->setRequestFinishedCallback(
        [response, this, handler = std::move(completedHandler), silent,
         trace](auto) mutable {
        const auto httpCode = response->getHTTPCode();
        const auto body = response->readAll<std::string>();

        if (httpCode != 200) {
            if (handler) {
                handler({});
            }

            // Token has expired?
            if (httpCode == 422) {
                BasicUI::CallAfter([this, trace] { UnlinkAccount(trace); });
            } else {
                SafePublish({ {}, body, trace, false, silent });
            }

            return;
        }
        ParseTokenResponse(body, std::move(handler), {}, trace, silent);
    });
}

void OAuthService::ParseTokenResponse(std::string_view body,
                                      AuthSuccessCallback successCallback,
                                      AuthFailureCallback failureCallback,
                                      AudiocomTrace trace,
                                      bool silent)
{
    rapidjson::Document document;
    document.Parse(body.data(), body.size());

    if (!document.IsObject()) {
        if (failureCallback) {
            failureCallback(200, body);
        }

        SafePublish({ {}, body, trace, false, silent });
        return;
    }

    const auto tokenType = document["token_type"].GetString();
    const auto accessToken = document["access_token"].GetString();
    const auto expiresIn = document["expires_in"].GetInt64();
    const auto newRefreshToken = document["refresh_token"].GetString();

    {
        std::lock_guard<std::recursive_mutex> lock(mMutex);

        mAccessToken = std::string(tokenType) + " " + accessToken;
        mTokenExpirationTime
            =Clock::now() + std::chrono::seconds(expiresIn);
    }

    BasicUI::CallAfter(
        [token = std::string(newRefreshToken)]()
    {
        // At this point access token is already written,
        // only refresh token is updated.
        refreshToken.Write(token);
        gPrefs->Flush();
    });

    if (successCallback) {
        successCallback(mAccessToken);
    }

    // The callback only needs the access token, so invoke it immediately.
    // Networking is thread safe
    SafePublish({ mAccessToken, {}, trace, true, silent });
}

void OAuthService::SafePublish(const AuthStateChangedMessage& message)
{
    BasicUI::CallAfter([this, message]() { Publish(message); });
}

OAuthService& GetOAuthService()
{
    static OAuthService service;
    return service;
}

namespace {
class OAuthServiceSettingsResetHandler final : public PreferencesResetHandler
{
public:
    void OnSettingResetBegin() override
    {
    }

    void OnSettingResetEnd() override
    {
        GetOAuthService().UnlinkAccount(AudiocomTrace::ignore);
        refreshToken.Invalidate();
    }
};

static PreferencesResetHandler::Registration<OAuthServiceSettingsResetHandler>
resetHandler;
}
} // namespace audacity::cloud::audiocom
