/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ServiceConfig.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <chrono>
#include <string>
#include <vector>
#include <rapidjson/fwd.h>

namespace audacity::cloud::audiocom
{

//! Configuration for the audio.com
class CLOUD_AUDIOCOM_API ServiceConfig final
{
public:
   ServiceConfig();
   //! API endpoint
   std::string GetAPIEndpoint() const;
   //! Page to open in browser to initiate OAuth
   std::string GetOAuthLoginPage() const;
   //! OAuth2 client ID
   std::string GetOAuthClientID() const;
   //! OAuth2 client secret
   std::string GetOAuthClientSecret() const;
   //! OAuth2 redirect URL. Only used to satisfy the protocol
   std::string GetOAuthRedirectURL() const;
   //! Helper to construct the full URLs for the API
   std::string GetAPIUrl(std::string_view apiURI) const;
   //! Helper to construct the page URL for the anonymous upload last stage
   std::string GetFinishUploadPage(std::string_view audioID, std::string_view token) const;
   //! Helper to construct the page URL for the authorised upload
   std::string GetAudioURL(std::string_view userSlug, std::string_view audioSlug) const;
   //! Timeout between progress callbacks
   std::chrono::milliseconds GetProgressCallbackTimeout() const;
   //! Preferred audio format
   std::vector<std::string> GetPreferredAudioFormats() const;
   //! Export configuration suitable for the mime type provided
   rapidjson::Document GetExportConfig(const std::string& exporterName) const;
   //! Return the mime type server should store the file. This is a requirement
   //! from audiocom
   std::string GetDownloadMime() const;
   //! Returns the preferred language
   std::string GetAcceptLanguageValue() const;

private:
   std::string mApiEndpoint;
   std::string mOAuthClientID;
   std::string mOAuthClientSecret;
   std::string mOAuthRedirectURL;
   std::string mOAuthLoginPage;
   std::string mFinishUploadPage;
   std::string mAudioURL;
   std::string mPreferredMimeType;
};

//! Returns the instance of the ServiceConfig
CLOUD_AUDIOCOM_API const ServiceConfig& GetServiceConfig();
} // namespace audacity::cloud::audiocom
