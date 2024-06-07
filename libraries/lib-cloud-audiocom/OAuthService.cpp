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

namespace audacity::cloud::audiocom
{
namespace
{

StringSetting refreshToken { L"/cloud/audiocom/refreshToken", "" };

const std::string_view uriPrefix = "audacity://link";
const std::string_view usernamePrefix = "username=";
const std::string_view passwordPrefix = "password=";
const std::string_view tokenPrefix = "token=";
const std::string_view authorizationCodePrefix = "authorization_code=";

void WriteCommonFields(
   rapidjson::Document& document, std::string_view grantType, std::string_view scope)
{
   using namespace rapidjson;

   document.AddMember(
      "grant_type", StringRef(grantType.data(), grantType.size()),
      document.GetAllocator());

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

   document.AddMember(
      "scope", StringRef(scope.data(), scope.size()), document.GetAllocator());
}

} // namespace

void OAuthService::ValidateAuth(
   std::function<void(std::string_view)> completedHandler, AudiocomTrace trace,
   bool silent)
{
   if (HasAccessToken() || !HasRefreshToken())
   {
      if (completedHandler)
         completedHandler(GetAccessToken());
      return;
   }

   AuthoriseRefreshToken(
      GetServiceConfig(), trace, std::move(completedHandler), silent);
}

bool OAuthService::HandleLinkURI(
   std::string_view uri, AudiocomTrace trace,
   std::function<void(std::string_view)> completedHandler)
{
   if (!IsPrefixedInsensitive(uri, uriPrefix))
   {
      if (completedHandler)
         completedHandler({});
      return false;
   }

   // It was observed, that sometimes link is passed as audacity://link/
   // This is valid trace URI point of view, but we need to handle it separately
   const auto argsStart = uri.find("?");

   if (argsStart == std::string_view::npos)
   {
      if (completedHandler)
         completedHandler({});
      return false;
   }

   // Length is handled in IsPrefixed
   auto args = uri.substr(argsStart + 1);

   std::string_view token;
   std::string_view username;
   std::string_view password;
   std::string_view authorizationCode;

   while (!args.empty())
   {
      const auto nextArg = args.find('&');

      const auto arg = args.substr(0, nextArg);
      args = nextArg == std::string_view::npos ? "" : args.substr(nextArg + 1);

      if (IsPrefixed(arg, usernamePrefix))
         username = arg.substr(usernamePrefix.length());
      else if (IsPrefixed(arg, passwordPrefix))
         password = arg.substr(passwordPrefix.length());
      else if (IsPrefixed(arg, tokenPrefix))
         token = arg.substr(tokenPrefix.length());
      else if (IsPrefixed(arg, authorizationCodePrefix))
         authorizationCode = arg.substr(authorizationCodePrefix.length());
   }

   // We have a prioritized list of authorization methods
   if (!authorizationCode.empty())
   {
      AuthoriseCode(
         GetServiceConfig(), authorizationCode, trace,
         std::move(completedHandler));
   }
   else if (!token.empty())
   {
      AuthoriseRefreshToken(
         GetServiceConfig(), token, trace, std::move(completedHandler), false);
   }
   else if (!username.empty() && !password.empty())
   {
      AuthorisePassword(
         GetServiceConfig(), audacity::UrlDecode(std::string(username)),
         audacity::UrlDecode(std::string(password)), trace,
         std::move(completedHandler));
   }
   else
   {
      if (completedHandler)
         completedHandler({});

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
   const ServiceConfig& config, std::string_view authorizationCode,
   AudiocomTrace trace, std::function<void(std::string_view)> completedHandler)
{
   using namespace rapidjson;

   Document document;
   document.SetObject();

   WriteCommonFields(document, "authorization_code", "all");

   document.AddMember(
      "code", StringRef(authorizationCode.data(), authorizationCode.size()),
      document.GetAllocator());

   const auto redirectURI = config.GetOAuthRedirectURL();

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

   if (Clock::now() < mTokenExpirationTime)
      return mAccessToken;

   return {};
}

void OAuthService::DoAuthorise(
   const ServiceConfig& config, std::string_view payload, AudiocomTrace trace,
   std::function<void(std::string_view)> completedHandler, bool silent)
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
       trace](auto) {
         const auto httpCode = response->getHTTPCode();
         const auto body = response->readAll<std::string>();

         if (httpCode != 200)
         {
            if (handler)
               handler({});

            // Token has expired?
            if (httpCode == 422)
               BasicUI::CallAfter([this, trace] { UnlinkAccount(trace); });
            else
               SafePublish({ {}, body, trace, false, silent });

            return;
         }

         rapidjson::Document document;
         document.Parse(body.data(), body.size());

         if (!document.IsObject())
         {
            if (handler)
               handler({});

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
            mTokenExpirationTime =
               Clock::now() + std::chrono::seconds(expiresIn);
         }

         BasicUI::CallAfter(
            [token = std::string(newRefreshToken)]()
            {
               // At this point access token is already written,
               // only refresh token is updated.
               refreshToken.Write(token);
               gPrefs->Flush();
            });

         if (handler)
            handler(mAccessToken);

         // The callback only needs the access token, so invoke it immediately.
         // Networking is thread safe
         SafePublish({ mAccessToken, {}, trace, true, silent });
      });
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

namespace
{

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
