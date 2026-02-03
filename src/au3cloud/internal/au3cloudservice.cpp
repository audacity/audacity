/*
* Audacity: A Digital Audio Editor
*/
#include "au3cloudservice.h"

#include <QApplication>
#include <QTimer>

#include <string>

#include "au3-cloud-audiocom/OAuthService.h"
#include "au3-cloud-audiocom/UserService.h"
#include "au3-cloud-audiocom/ServiceConfig.h"
#include "au3-import-export/ExportUtils.h"
#include "au3-preferences/Prefs.h"

using namespace au::au3cloud;

void Au3CloudService::init()
{
    auto& serviceConfig = audacity::cloud::audiocom::GetServiceConfig();
    m_replyHandler = new OAuthHttpServerReplyHandler(iocContext(), this);
    m_replyHandler->setRedirectUrl(QUrl(serviceConfig.GetOAuthRedirectURL().c_str()));
    connect(m_replyHandler, &OAuthHttpServerReplyHandler::callbackReceived,
            this, [this](const QVariantMap& data) {
        // Extract authorization code from callback
        std::string code = data.value("code").toString().toStdString();
        std::string error = data.value("error").toString().toStdString();

        if (code.empty() || !error.empty()) {
            m_authState.set(AuthState::NotAuthorized);
            return;
        }

        auto& oauthService = audacity::cloud::audiocom::GetOAuthService();
        oauthService.AuthorizeCode(
            code,
            m_replyHandler->callback().toStdString(),
            AudiocomTrace::ignore,
            [this](auto token)
        {
            if (token.empty()) {
                m_authState.set(AuthState::NotAuthorized);
            }
        });
    });

    auto& oauthService = audacity::cloud::audiocom::GetOAuthService();
    m_authState.set(oauthService.HasAccessToken() ? AuthState::Authorized : AuthState::NotAuthorized);
    m_authSubscription
        = audacity::cloud::audiocom::GetOAuthService().Subscribe([this](const audacity::cloud::audiocom::AuthStateChangedMessage& message)
    {
        m_authState.set(
            message.authorised ? AuthState::Authorized : AuthState::NotAuthorized);

        auto& service = audacity::cloud::audiocom::GetUserService();
        message.authorised ? service.UpdateUserData() : service.ClearUserData();
    });
}

void Au3CloudService::registerWithPassword(const std::string& email, const std::string& password)
{
    m_authState.set(AuthState::Authorizing);

    auto& oauthService = audacity::cloud::audiocom::GetOAuthService();
    oauthService.Register(email, password,
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
    m_authState.set(AuthState::Authorizing);
    interactive()->openUrl(buildOAuthRequestURL(provider));
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

void Au3CloudService::setSendAnonymousUsageInfo(bool send)
{
    SendAnonymousUsageInfo->Write(send);
}

bool Au3CloudService::getSendAnonymousUsageInfo() const
{
    return SendAnonymousUsageInfo->Read();
}

std::string Au3CloudService::buildOAuthRequestURL(const std::string& provider)
{
    auto& serviceConfig = audacity::cloud::audiocom::GetServiceConfig();
    const std::map<std::string, std::string> params = {
        { "authclient", provider },
        { "response_type", "code" },
        { "client_id", serviceConfig.GetOAuthClientID() },
        { "client_secret", serviceConfig.GetOAuthClientSecret() },
        { "redirect_uri", m_replyHandler->callback().toStdString() }
    };

    std::string url = serviceConfig.GetAPIUrl("/auth/authorize?");
    for (const auto& [key, value] : params) {
        url.append(key);
        url.append("=");
        url.append(value);
        url.append("&");
    }
    url.pop_back(); // Remove last '&'
    return url;
}
