/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  UserService.cpp

  Dmitry Vedenko

**********************************************************************/

#include "UserService.h"

#include <memory>
#include <vector>

#include <wx/file.h>

#include <rapidjson/document.h>

#include "ServiceConfig.h"
#include "OAuthService.h"

#include "BasicUI.h"
#include "FileNames.h"
#include "Observer.h"
#include "Prefs.h"

#include "IResponse.h"
#include "NetworkManager.h"
#include "Request.h"

#include "CodeConversions.h"

namespace audacity::cloud::audiocom {
namespace {
wxString MakeAvatarPath()
{
    const wxFileName avatarFileName(FileNames::ConfigDir(), "avatar");
    return avatarFileName.GetFullPath();
}

StringSetting userId { L"/cloud/audiocom/userId", "" };
StringSetting userName { L"/cloud/audiocom/userName", "" };
StringSetting displayName { L"/cloud/audiocom/displayName", "" };
StringSetting avatarEtag { L"/cloud/audiocom/avatarEtag", "" };

Observer::Subscription authStateChangedSubscription
    =GetOAuthService().Subscribe(
          [](const auto& state)
{
    if (state.authorised) {
        GetUserService().UpdateUserData();
    } else {
        GetUserService().ClearUserData();
    }
});
} // namespace

void UserService::UpdateUserData()
{
    auto& oauthService = GetOAuthService();

    if (!oauthService.HasAccessToken()) {
        return;
    }

    using namespace audacity::network_manager;

    Request request(GetServiceConfig().GetAPIUrl("/me"));

    request.setHeader(
        common_headers::Authorization,
        std::string(oauthService.GetAccessToken()));

    request.setHeader(
        common_headers::Accept, common_content_types::ApplicationJson);

    auto response = NetworkManager::GetInstance().doGet(request);

    response->setRequestFinishedCallback(
        [response, this](auto)
    {
        const auto httpCode = response->getHTTPCode();

        if (httpCode != 200) {
            return;
        }

        const auto body = response->readAll<std::string>();

        using namespace rapidjson;

        Document document;
        document.Parse(body.data(), body.size());

        if (!document.IsObject()) {
            return;
        }

        const auto id = document["id"].GetString();
        const auto username = document["username"].GetString();
        const auto avatar = document["avatar"].GetString();
        const auto profileName = document["profile"]["name"].GetString();

        BasicUI::CallAfter(
            [this,
             id = std::string(id),
             username = std::string(username),
             profileName = std::string(profileName),
             avatar = std::string(avatar)]()
        {
            userId.Write(audacity::ToWXString(id));
            userName.Write(audacity::ToWXString(username));
            displayName.Write(audacity::ToWXString(profileName));

            gPrefs->Flush();

            DownloadAvatar(avatar);

            Publish({});
        });
    });
}

void UserService::ClearUserData()
{
    BasicUI::CallAfter(
        [this]()
    {
        // No valid data was present, do not spam Publish()
        if (GetUserSlug().empty()) {
            return;
        }

        userId.Write({});
        userName.Write({});
        displayName.Write({});
        avatarEtag.Write({});

        gPrefs->Flush();

        Publish({});
    });
}

UserService& GetUserService()
{
    static UserService userService;
    return userService;
}

void UserService::DownloadAvatar(std::string_view url)
{
    const auto avatarPath = MakeAvatarPath();
    const auto avatarTempPath = avatarPath + ".tmp";

    if (url.empty()) {
        if (wxFileExists(avatarPath)) {
            wxRemoveFile(avatarPath);
        }

        return;
    }

    std::shared_ptr<wxFile> avatarFile = std::make_shared<wxFile>();

    if (!avatarFile->Create(avatarTempPath, true)) {
        return;
    }

    using namespace audacity::network_manager;

    auto request = Request(std::string(url));

    const auto etag = audacity::ToUTF8(avatarEtag.Read());

    // If ETag is present - use it to prevent re-downloading the same file
    if (!etag.empty() && wxFileExists(avatarPath)) {
        request.setHeader(common_headers::IfNoneMatch, etag);
    }

    auto response = NetworkManager::GetInstance().doGet(request);

    response->setOnDataReceivedCallback(
        [response, avatarFile](auto)
    {
        std::vector<char> buffer(response->getBytesAvailable());

        size_t bytes = response->readData(buffer.data(), buffer.size());

        avatarFile->Write(buffer.data(), buffer.size());
    });

    response->setRequestFinishedCallback(
        [response, avatarFile, avatarPath, avatarTempPath, this](auto)
    {
        avatarFile->Close();

        const auto httpCode = response->getHTTPCode();

        if (httpCode != 200) {
            // For any response except 200 just remove the temp file
            wxRemoveFile(avatarTempPath);
            return;
        }

        const auto etag = response->getHeader("ETag");
        const auto oldPath = avatarPath + ".old";

        if (wxFileExists(avatarPath)) {
            if (!wxRenameFile(avatarPath, oldPath)) {
                return;
            }
        }

        if (!wxRenameFile(avatarTempPath, avatarPath)) {
            // Try at least to get it back...
            wxRenameFile(oldPath, avatarPath);
            return;
        }

        if (wxFileExists(oldPath)) {
            wxRemoveFile(oldPath);
        }

        BasicUI::CallAfter(
            [this, etag]()
        {
            avatarEtag.Write(etag);
            gPrefs->Flush();

            Publish({});
        });
    });
}

wxString UserService::GetUserId() const
{
    return userId.Read();
}

wxString UserService::GetDisplayName() const
{
    return displayName.Read();
}

wxString UserService::GetUserSlug() const
{
    return userName.Read();
}

wxString UserService::GetAvatarPath() const
{
    auto path = MakeAvatarPath();

    if (!wxFileExists(path)) {
        return {}
    }

    return path;
}
} // namespace audacity::cloud::audiocom
