/*
* Audacity: A Digital Audio Editor
*/
#include <QApplication>
#include <QTimer>

#include <string>

#include "framework/global/settings.h"

#include "au3-cloud-audiocom/OAuthService.h"
#include "au3-cloud-audiocom/UserService.h"
#include "au3-import-export/ExportUtils.h"

#include "au3cloudservice.h"

using namespace au::au3cloud;

namespace {
const muse::Settings::Key CLOUD_AUTH_LINK_KEY("cloud", "cloud/authLinkUrl");
}

void Au3CloudService::init()
{
    auto& oauthService = audacity::cloud::audiocom::GetOAuthService();
    m_authState.set(oauthService.HasAccessToken() ? AuthState::Authorized : AuthState::NotAuthorized);
    m_authSubscription
        = audacity::cloud::audiocom::GetOAuthService().Subscribe([this](const audacity::cloud::audiocom::AuthStateChangedMessage& message)
    {
        m_authState.set(
            message.authorised ? AuthState::Authorized : AuthState::NotAuthorized);
    });

    multiInstancesProvider()->resourceChanged().onReceive(this, [this](const std::string& message){
        if (message != CLOUD_AUTH_LINK_KEY.key) {
            return;
        }

        const std::string linkUrl = muse::settings()->value(CLOUD_AUTH_LINK_KEY).toString();
        if (!linkUrl.empty()) {
            audacity::cloud::audiocom::GetOAuthService().HandleLinkURI(
                linkUrl, AudiocomTrace::ignore, {});
        }
    });
}

void Au3CloudService::signInWithPassword(const std::string& email, const std::string& password)
{
    m_authState.set(AuthState::Authorizing);

    auto& oauthService = audacity::cloud::audiocom::GetOAuthService();
    oauthService.Authorize(email, password,
                           [this](auto token)
    {
        if (token.empty()) {
            m_authState.set(AuthState::NotAuthorized);
            return;
        }

        m_authState.set(AuthState::Authorized);
    },
                           [this](auto, auto)
    {
        m_authState.set(AuthState::NotAuthorized);
    }, AudiocomTrace::ignore);
}

void Au3CloudService::signInWithSocial(const std::string& provider)
{
    interactive()->openUrl(audacity::cloud::audiocom::OAuthService::MakeOAuthRequestURL(provider));
}

void Au3CloudService::signOut()
{
    auto& oauthService = audacity::cloud::audiocom::GetOAuthService();
    oauthService.UnlinkAccount(AudiocomTrace::ignore);
    m_authState.set(AuthState::NotAuthorized);
}

muse::ValCh<AuthState> Au3CloudService::authState() const
{
    return m_authState;
}

std::string Au3CloudService::getAvatarPath() const
{
    const auto& userService = audacity::cloud::audiocom::GetUserService();
    return userService.GetAvatarPath().ToStdString();
}

std::string Au3CloudService::getDisplayName() const
{
    auto& userService = audacity::cloud::audiocom::GetUserService();
    return userService.GetDisplayName().IsEmpty() ? userService.GetUserSlug().ToStdString()
           : userService.GetDisplayName().ToStdString();
}
