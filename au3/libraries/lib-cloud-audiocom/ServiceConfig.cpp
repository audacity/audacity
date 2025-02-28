/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ServiceConfig.cpp

  Dmitry Vedenko

**********************************************************************/
#include "ServiceConfig.h"
#include "ExportUtils.h"
#include "Languages.h"

#include <cassert>
#include <stdexcept>
#include <string_view>

#include <rapidjson/document.h>

#include "CodeConversions.h"
#include "Prefs.h"

namespace audacity::cloud::audiocom {
namespace {
#define MTM_CAMPAIGN \
    "mtm_campaign=Audacity&mtm_source=Audacity-{version_number}&mtm_content={button_name}"

StringSetting audioComApiEndpoint { L"/CloudServices/AudioCom/ApiEndpoint",
                                    L"https://api.audio.com" };

StringSetting audioComOAuthClientID { L"/CloudServices/AudioCom/OAuthClientID",
                                      L"1741964426607541" };

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
    L"https://audio.com/audacity/link?clientId={auth_client_id}&" MTM_CAMPAIGN
};

StringSetting audioComFinishUploadPage {
    L"/CloudServices/AudioCom/FinishUploadPage",
    L"https://audio.com/audacity/upload?audioId={audio_id}&token={auth_token}&clientId={auth_client_id}&" MTM_CAMPAIGN
};

StringSetting audioComAuthWithRedirectURL {
    L"/CloudServices/AudioCom/AuthWithRedirectURL",
    L"https://audio.com/auth/check-and-redirect"
};

StringSetting audioComFrontendURL { L"/CloudServices/AudioCom/FrontendURL",
                                    L"https://audio.com" };

StringSetting audioComAudioDownloadMimeType {
    L"/CloudServices/AudioCom/DownloadMimeType", L"audio/x-wav"
};

std::string Substitute(
    std::string pattern,
    std::initializer_list<std::pair<std::string_view, std::string_view> >
    substitutions)
{
    for (auto& [key, value] : substitutions) {
        auto pos = pattern.find(key);

        while (pos > 0 && pos != std::string::npos)
        {
            // There is no need to check that pos + key.size() is valid, there
            // will be a zero terminator in the worst case.
            if (pattern[pos - 1] == '{' && pattern[pos + key.size()] == '}') {
                pattern.replace(pos - 1, key.size() + 2, value);
            }

            pos = pattern.find(key, pos + 1);
        }
    }

    return std::move(pattern);
}

std::string GetButtonName(AudiocomTrace trace)
{
    switch (trace) {
    case AudiocomTrace::ignore:
        return "ignore";
    case AudiocomTrace::ShareAudioButton:
        return "Share_Audio_Button";
    case AudiocomTrace::ShareAudioMenu:
        return "Share_Audio_Menu";
    case AudiocomTrace::ShareAudioExportMenu:
        return "Share_Audio_Export_Menu";
    case AudiocomTrace::ShareAudioExportExtraMenu:
        return "Share_Audio_Export_Extra_Menu";
    case AudiocomTrace::SaveToCloudMenu:
        return "Save_to_Cloud_Menu";
    case AudiocomTrace::SaveProjectSaveToCloudMenu:
        return "Save_Project_Save_to_Cloud_Menu";
    case AudiocomTrace::PrefsPanel:
        return "Prefs_Panel";
    case AudiocomTrace::ProjectOpenedAndUploadResumed:
        return "Project_Opened_And_Upload_Resumed";
    case AudiocomTrace::UpdateCloudAudioPreviewMenu:
        return "Update_Cloud_Audio_Preview_Menu";
    case AudiocomTrace::LinkAudiocomAccountHelpMenu:
        return "Link_Audiocom_Account_Help_Menu";
    case AudiocomTrace::OpenFromCloudMenu:
        return "Open_From_Cloud_Menu";
    }

    assert(false);
    return {};
}
} // namespace

ServiceConfig::ServiceConfig()
{
    mApiEndpoint         = audacity::ToUTF8(audioComApiEndpoint.Read());
    mOAuthClientID       = audacity::ToUTF8(audioComOAuthClientID.Read());
    mOAuthClientSecret   = audacity::ToUTF8(audioComOAuthClientSecret.Read());
    mOAuthRedirectURL    = audacity::ToUTF8(audioComOAuthRedirectURL.Read());
    mAuthWithRedirectURL = audacity::ToUTF8(audioComAuthWithRedirectURL.Read());
    mOAuthLoginPage      = audacity::ToUTF8(audioComOAuthLoginPage.Read());
    mFinishUploadPage    = audacity::ToUTF8(audioComFinishUploadPage.Read());
    mFrontendURL         = audacity::ToUTF8(audioComFrontendURL.Read());
    mPreferredMimeType   = audacity::ToUTF8(audioComAudioDownloadMimeType.Read());
}

std::string ServiceConfig::GetAPIEndpoint() const
{
    return mApiEndpoint;
}

std::string ServiceConfig::GetOAuthLoginPage(AudiocomTrace trace) const
{
    return Substitute(
        mOAuthLoginPage,
        { { "auth_client_id", GetOAuthClientID() },
            { "version_number", audacity::ToUTF8(AUDACITY_VERSION_STRING) },
            { "button_name", GetButtonName(trace) } });
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

std::string ServiceConfig::GetAuthWithRedirectURL() const
{
    return mAuthWithRedirectURL;
}

std::string ServiceConfig::GetAPIUrl(std::string_view apiURI) const
{
    return mApiEndpoint + std::string(apiURI);
}

std::string ServiceConfig::GetFinishUploadPage(
    std::string_view audioID, std::string_view token, AudiocomTrace trace) const
{
    return Substitute(
        mFinishUploadPage,
        { { "audio_id", audioID },
            { "auth_token", token },
            { "auth_client_id", mOAuthClientID },
            { "version_number", audacity::ToUTF8(AUDACITY_VERSION_STRING) },
            { "button_name", GetButtonName(trace) } });
}

std::string ServiceConfig::GetAudioURL(
    std::string_view userSlug, std::string_view audioSlug,
    AudiocomTrace trace) const
{
    return Substitute(
        "{frontend_url}/{user_slug}/audio/{audio_slug}/edit?" MTM_CAMPAIGN,
        { { "frontend_url", mFrontendURL },
            { "user_slug", userSlug },
            { "audio_slug", audioSlug },
            { "version_number", audacity::ToUTF8(AUDACITY_VERSION_STRING) },
            { "button_name", GetButtonName(trace) } });
}

std::chrono::milliseconds ServiceConfig::GetProgressCallbackTimeout() const
{
    return std::chrono::seconds(3);
}

std::vector<std::string>
ServiceConfig::GetPreferredAudioFormats(bool preferLossless) const
{
    if (preferLossless) {
        return { "audio/x-wavpack", "audio/x-flac", "audio/x-wav" }
    } else {
        return { "audio/mpeg" }
    }
}

rapidjson::Document
ServiceConfig::GetExportConfig(const std::string& mimeType) const
{
    if (mimeType == "audio/x-wavpack") {
        rapidjson::Document config;
        config.SetObject();
        config.AddMember("quality", rapidjson::Value(2), config.GetAllocator());
        config.AddMember("bit_rate", rapidjson::Value(40), config.GetAllocator());
        config.AddMember("bit_depth", 24, config.GetAllocator());
        config.AddMember("hybrid_mode", false, config.GetAllocator());
        return config;
    } else if (mimeType == "audio/x-flac") {
        rapidjson::Document config;
        config.SetObject();
        config.AddMember(
            "bit_depth", rapidjson::Value(24), config.GetAllocator());
        config.AddMember("level", rapidjson::Value(5), config.GetAllocator());
    } else if (mimeType == "audio/x-wav") {
        return {};
    } else if (mimeType == "audio/mpeg") {
        rapidjson::Document config;
        config.SetObject();
        config.AddMember("mode", rapidjson::Value("VBR"), config.GetAllocator());
        config.AddMember("quality", rapidjson::Value(5), config.GetAllocator());
        return config;
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

    if (language.Contains(L"-") && language.Length() > 2) {
        return wxString::Format(
            "%s;q=1.0, %s;q=0.7, *;q=0.5", language, language.Left(2))
               .ToStdString();
    } else {
        return wxString::Format("%s;q=1.0, *;q=0.5", language).ToStdString();
    }
}

std::string ServiceConfig::GetCreateProjectUrl() const
{
    return GetAPIUrl("/project");
}

std::string
ServiceConfig::GetCreateSnapshotUrl(std::string_view projectId) const
{
    return Substitute(
        "{api_url}/project/{project_id}/snapshot",
        { { "api_url", mApiEndpoint }, { "project_id", projectId } });
}

std::string ServiceConfig::GetSnapshotSyncUrl(
    std::string_view projectId, std::string_view snapshotId) const
{
    return Substitute(
        "{api_url}/project/{project_id}/snapshot/{snapshot_id}/sync",
        { { "api_url", mApiEndpoint },
            { "project_id", projectId },
            { "snapshot_id", snapshotId } });
}

std::string ServiceConfig::GetProjectsUrl(
    int page, int pageSize, std::string_view searchTerm) const
{
    if (searchTerm.empty()) {
        return Substitute(
            "{api_url}/project?page={page}&per-page={page_size}",
            { { "api_url", mApiEndpoint },
                { "page", std::to_string(page) },
                { "page_size", std::to_string(pageSize) }, });
    }

    return Substitute(
        "{api_url}/project?page={page}&per-page={page_size}&q={search_term}",
        { { "api_url", mApiEndpoint },
            { "page", std::to_string(page) },
            { "page_size", std::to_string(pageSize) },
            { "search_term", searchTerm }, });
}

std::string ServiceConfig::GetProjectInfoUrl(std::string_view projectId) const
{
    return Substitute(
        "{api_url}/project/{project_id}", {
            { "api_url", mApiEndpoint },
            { "project_id", projectId },
        });
}

std::string ServiceConfig::GetSnapshotInfoUrl(
    std::string_view projectId, std::string_view snapshotId) const
{
    return Substitute(
        "{api_url}/project/{project_id}/snapshot/{snapshot_id}?expand=blocks,file_url",
        {
            { "api_url", mApiEndpoint },
            { "project_id", projectId },
            { "snapshot_id", snapshotId },
        });
}

std::string ServiceConfig::GetDeleteSnapshotUrl(
    std::string_view projectId, std::string_view snapshotId) const
{
    return Substitute(
        "{api_url}/project/{project_id}/snapshot/{snapshot_id}",
        {
            { "api_url", mApiEndpoint },
            { "project_id", projectId },
            { "snapshot_id", snapshotId },
        });
}

std::string ServiceConfig::GetNetworkStatsUrl(std::string_view projectId) const
{
    return Substitute(
        "{api_url}/project/{project_id}/network-stats",
        {
            { "api_url", mApiEndpoint },
            { "project_id", projectId },
        });
}

std::string ServiceConfig::GetProjectPagePath(
    std::string_view userSlug, std::string_view projectSlug,
    AudiocomTrace trace) const
{
    return Substitute(
        "/{user_slug}/projects/{project_slug}&" MTM_CAMPAIGN,
        {
            { "user_slug", userSlug },
            { "project_slug", projectSlug },
            { "version_number", audacity::ToUTF8(AUDACITY_VERSION_STRING) },
            { "button_name", GetButtonName(trace) },
        });
}

std::string ServiceConfig::GetProjectsPagePath(
    std::string_view userSlug, AudiocomTrace trace) const
{
    return Substitute(
        "/{user_slug}/projects?" MTM_CAMPAIGN,
        {
            { "user_slug", userSlug },
            { "version_number", audacity::ToUTF8(AUDACITY_VERSION_STRING) },
            { "button_name", GetButtonName(trace) },
        });
}

const ServiceConfig& GetServiceConfig()
{
    static ServiceConfig config;
    return config;
}
} // namespace audacity::cloud::audiocom
