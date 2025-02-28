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

enum class AudiocomTrace;

namespace audacity::cloud::audiocom {
//! Configuration for the audio.com
class CLOUD_AUDIOCOM_API ServiceConfig final
{
public:
    ServiceConfig();
    //! API endpoint
    std::string GetAPIEndpoint() const;
    //! Page to open in browser to initiate OAuth
    std::string GetOAuthLoginPage(AudiocomTrace) const;
    //! OAuth2 client ID
    std::string GetOAuthClientID() const;
    //! OAuth2 client secret
    std::string GetOAuthClientSecret() const;
    //! OAuth2 redirect URL. Only used to satisfy the protocol
    std::string GetOAuthRedirectURL() const;
    //! Audio.com authorization API to automatically login current user
    //  in the default browser when opening the project from audacity
    std::string GetAuthWithRedirectURL() const;
    //! Helper to construct the full URLs for the API
    std::string GetAPIUrl(std::string_view apiURI) const;
    //! Helper to construct the page URL for the anonymous upload last stage
    std::string GetFinishUploadPage(
        std::string_view audioID, std::string_view token, AudiocomTrace) const;
    //! Helper to construct the page URL for the authorised upload
    std::string GetAudioURL(
        std::string_view userSlug, std::string_view audioSlug, AudiocomTrace) const;
    //! Timeout between progress callbacks
    std::chrono::milliseconds GetProgressCallbackTimeout() const;
    //! Preferred audio format
    std::vector<std::string> GetPreferredAudioFormats(bool preferLossless = true) const;
    //! Export configuration suitable for the mime type provided
    rapidjson::Document GetExportConfig(const std::string& exporterName) const;
    //! Return the mime type server should store the file. This is a requirement
    //! from audiocom
    std::string GetDownloadMime() const;
    //! Returns the preferred language
    std::string GetAcceptLanguageValue() const;

    std::string GetCreateProjectUrl() const;
    std::string GetCreateSnapshotUrl(std::string_view projectId) const;
    std::string GetSnapshotSyncUrl(
        std::string_view projectId, std::string_view snapshotId) const;
    std::string GetProjectsUrl(int page, int pageSize, std::string_view searchTerm) const;

    std::string GetProjectInfoUrl(std::string_view projectId) const;
    std::string GetSnapshotInfoUrl(
        std::string_view projectId, std::string_view snapshotId) const;
    std::string GetDeleteSnapshotUrl(
        std::string_view projectId, std::string_view snapshotId) const;

    std::string GetNetworkStatsUrl(std::string_view projectId) const;
    std::string GetProjectPagePath(std::string_view userSlug, std::string_view projectSlug, AudiocomTrace) const;
    std::string
    GetProjectsPagePath(std::string_view userSlug, AudiocomTrace) const;

private:
    std::string mApiEndpoint;
    std::string mOAuthClientID;
    std::string mOAuthClientSecret;
    std::string mOAuthRedirectURL;
    std::string mOAuthLoginPage;
    std::string mAuthWithRedirectURL;
    std::string mFinishUploadPage;
    std::string mFrontendURL;
    std::string mPreferredMimeType;
};

//! Returns the instance of the ServiceConfig
CLOUD_AUDIOCOM_API const ServiceConfig& GetServiceConfig();
} // namespace audacity::cloud::audiocom
