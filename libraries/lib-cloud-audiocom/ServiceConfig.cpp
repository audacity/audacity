/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ServiceConfig.cpp

  Dmitry Vedenko

**********************************************************************/
#include "ServiceConfig.h"
#include "Languages.h"

#include <cassert>
#include <stdexcept>
#include <string_view>

#include <rapidjson/document.h>

#include "Prefs.h"
#include "CodeConversions.h"

namespace audacity::cloud::audiocom
{
namespace
{
StringSetting audioComApiEndpoint {
   L"/CloudServices/AudioCom/ApiEndpoint",
   L"https://api.audio.com"
};

StringSetting audioComOAuthClientID {
   L"/CloudServices/AudioCom/OAuthClientID",
   L"1741964426607541"
};

StringSetting audioComOAuthClientSecret {
   L"/CloudServices/AudioCom/OAuthClientSecret",
   L"shKqnY2sLTfRK7hztwzNEVxnmhJfOy1i"
};

StringSetting audioComOAuthRedirectURL {
   L"/CloudServices/AudioCom/OAuthRedirectURL",
   L"https://audio.com/auth/sign-in/success"
};

StringSetting audioComOAuthLoginPage {
   L"/CloudServices/AudioCom/OAuthLoginPage",
   L"https://audio.com/audacity/link?clientId={auth_client_id}"
};

StringSetting audioComFinishUploadPage {
   L"/CloudServices/AudioCom/FinishUploadPage",
   L"https://audio.com/audacity/upload?audioId={audio_id}&token={auth_token}&clientId={auth_client_id}"
};

StringSetting audioComAudioURL {
   L"/CloudServices/AudioCom/AudioURL",
   L"https://audio.com/{user_slug}/audio/{audio_slug}/edit"
};

StringSetting audioComAudioDownloadMimeType {
   L"/CloudServices/AudioCom/DownloadMimeType",
   L"audio/x-wav"
};

std::string Substitute(std::string pattern, std::initializer_list<std::pair<std::string_view, std::string_view>> substitutions)
{
   for(auto& [key, value] : substitutions)
   {
      auto pos = pattern.find(key);

      if (pos > 0 && pos != std::string::npos)
      {
         // There is no need to check that pos + key.size() is valid, there
         // will be a zero terminator in the worst case.
         if (pattern[pos - 1] == '{' && pattern[pos + key.size()] == '}')
            pattern.replace(pos - 1, key.size() + 2, value);
      }
   }

   return std::move(pattern);
}

} // namespace

ServiceConfig::ServiceConfig()
{
   mApiEndpoint = audacity::ToUTF8(audioComApiEndpoint.Read());
   mOAuthClientID = audacity::ToUTF8(audioComOAuthClientID.Read());
   mOAuthClientSecret = audacity::ToUTF8(audioComOAuthClientSecret.Read());
   mOAuthRedirectURL = audacity::ToUTF8(audioComOAuthRedirectURL.Read());
   mOAuthLoginPage = audacity::ToUTF8(audioComOAuthLoginPage.Read());
   mFinishUploadPage = audacity::ToUTF8(audioComFinishUploadPage.Read());
   mAudioURL = audacity::ToUTF8(audioComAudioURL.Read());
   mPreferredMimeType = audacity::ToUTF8(audioComAudioDownloadMimeType.Read());
}

std::string ServiceConfig::GetAPIEndpoint() const
{
   return mApiEndpoint;
}

std::string ServiceConfig::GetOAuthLoginPage() const
{
   return Substitute(
      mOAuthLoginPage, { { "auth_client_id", GetOAuthClientID() } });
}

std::string ServiceConfig::GetOAuthClientID() const
{
   return mOAuthClientID;
}

std::string ServiceConfig::GetOAuthClientSecret() const
{
   return mOAuthClientSecret;
}

std::string ServiceConfig::GetOAuthRedirectURL() const
{
   return mOAuthRedirectURL;
}

std::string ServiceConfig::GetAPIUrl(std::string_view apiURI) const
{
   return mApiEndpoint + std::string(apiURI);
}

std::string ServiceConfig::GetFinishUploadPage(
   std::string_view audioID, std::string_view token) const
{
   return Substitute(
      mFinishUploadPage, { { "audio_id", audioID },
                           { "auth_token", token },
                           { "auth_client_id", mOAuthClientID } });
}

std::string ServiceConfig::GetAudioURL(
   std::string_view userSlug, std::string_view audioSlug) const
{
   return Substitute(
      mAudioURL, { { "user_slug", userSlug }, { "audio_slug", audioSlug } });
}

std::chrono::milliseconds ServiceConfig::GetProgressCallbackTimeout() const
{
   return std::chrono::seconds(3);
}

std::vector<std::string> ServiceConfig::GetPreferredAudioFormats() const
{
   return { "audio/x-wavpack", "audio/x-flac", "audio/x-wav" };
}

rapidjson::Document ServiceConfig::GetExportConfig(const std::string& mimeType) const
{
   if(mimeType == "audio/x-wavpack")
   {
      rapidjson::Document config;
      config.SetObject();
      config.AddMember("quality", rapidjson::Value(2), config.GetAllocator());
      config.AddMember("bit_rate", rapidjson::Value(40), config.GetAllocator());
      config.AddMember("bit_depth", 24, config.GetAllocator());
      config.AddMember("hybrid_mode", false, config.GetAllocator());
      return config;
   }
   if(mimeType == "audio/x-flac")
   {
      rapidjson::Document config;
      config.SetObject();
      config.AddMember("bit_depth", rapidjson::Value(24), config.GetAllocator());
      config.AddMember("level", rapidjson::Value(5), config.GetAllocator());
   }
   if(mimeType == "audio/x-wav")
   {
      return {};
   }
   throw std::invalid_argument("unknown mime-type");
}


std::string ServiceConfig::GetDownloadMime() const
{
   return mPreferredMimeType;
}

std::string ServiceConfig::GetAcceptLanguageValue() const
{
   auto language = Languages::GetLang();

   if (language.Contains(L"-") && language.Length() > 2)
      return wxString::Format("%s;q=1.0, %s;q=0.7, *;q=0.5", language, language.Left(2)).ToStdString();
   else
      return wxString::Format("%s;q=1.0, *;q=0.5", language).ToStdString();
}

const ServiceConfig& GetServiceConfig()
{
   static ServiceConfig config;
   return config;
}

} // namespace audacity::cloud::audiocom
