/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ServiceConfig.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <chrono>
#include <string>
#include <string_view>

#include "MimeTypesList.h"

namespace cloud::audiocom
{

//! Configuration for the audio.com
class CLOUD_AUDIOCOM_API ServiceConfig final
{
public:
   //! API endpoint
   std::string_view GetAPIEndpoint() const;
   //! Page to open in browser to initiate OAuth 
   std::string_view GetOAuthLoginPage() const;
   //! OAuth2 client ID
   std::string_view GetOAuthClientID() const;
   //! OAuth2 redirect URL. Only used to satisfy the protocol
   std::string_view GetOAuthRedirectURL() const;
   //! Helper to construct the full URLs for the API
   std::string GetAPIUrl(std::string_view apiURI) const;
   //! Helper to construct the page URL for the anonymous upload last stage
   std::string GetFinishUploadPage(std::string_view audioID, std::string_view token) const;
   //! Timeout between progress callbacks
   std::chrono::milliseconds GetProgressCallbackTimeout() const;
   //! Preferred audio format
   MimeTypesList GetPreferredAudioFormats() const;
   //! Return the mime type server should store the file. This is a requirement from audiocom
   MimeType GetDownloadMime() const;
   //! Returns the preferred language
   std::string GetAcceptLanguageValue() const;
};

//! Returns the instance of the ServiceConfig
CLOUD_AUDIOCOM_API const ServiceConfig& GetServiceConfig();
} // namespace cloud::audiocom
