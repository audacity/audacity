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
#include "au3cloud/iauthorization.h"
#include "translation.h"

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
            m_authState.set(NotAuthorized(error));
            return;
        }

        auto& oauthService = audacity::cloud::audiocom::GetOAuthService();
        oauthService.AuthorizeCode(
            code,
            m_replyHandler->callback().toStdString(),
            AudiocomTrace::ignore,
            [this](auto token)
        {
            const auto AUTHORIZATION_FAILED = muse::qtrc("appshell/gettingstarted", "Authorization failed");
            if (token.empty()) {
                m_authState.set(NotAuthorized(AUTHORIZATION_FAILED.toStdString()));
            }
        });
    });

    auto& oauthService = audacity::cloud::audiocom::GetOAuthService();
    const auto NO_ACCESS_TOKEN = muse::qtrc("appshell/gettingstarted", "No access token");
    m_authState.set(oauthService.HasAccessToken() ? AuthState(Authorized()) : AuthState(NotAuthorized(NO_ACCESS_TOKEN.toStdString())));
    m_authSubscription
        = oauthService.Subscribe([this](const audacity::cloud::audiocom::AuthStateChangedMessage& message)
    {
        if (std::holds_alternative<Authorized>(m_authState.val) && message.authorised) {
            return;
        }

        if (std::holds_alternative<NotAuthorized>(m_authState.val) && !message.authorised) {
            return;
        }

        const auto NOT_AUTHORIZED = muse::qtrc("appshell/gettingstarted", "Not authorized");
        m_authState.set(
            message.authorised ? AuthState(Authorized()) : AuthState(NotAuthorized(NOT_AUTHORIZED.toStdString())));

        auto& service = audacity::cloud::audiocom::GetUserService();
        message.authorised ? service.UpdateUserData() : service.ClearUserData();
    });

    auto& userService = audacity::cloud::audiocom::GetUserService();
    m_userDataSubscription
        = userService.Subscribe(
              [this](const audacity::cloud::audiocom::UserDataChanged&) {
        auto& userService = audacity::cloud::audiocom::GetUserService();
        m_accountInfo.id = userService.GetUserId().ToStdString();
        m_accountInfo.userSlug = userService.GetUserSlug().ToStdString();
        m_accountInfo.displayName = userService.GetDisplayName().ToStdString();
        m_accountInfo.avatarPath = userService.GetAvatarPath().ToStdString();
    });
}

void Au3CloudService::registerWithPassword(const std::string& email, const std::string& password)
{
    m_authState.set(Authorizing());
    auto& oauthService = audacity::cloud::audiocom::GetOAuthService();
    oauthService.Register(email, password,
                          [this](auto token)
    {
        if (token.empty()) {
            const auto REGISTRATION_FAILED = muse::qtrc("appshell/gettingstarted", "Registration failed. Please try again.");
            m_authState.set(AuthState(NotAuthorized(REGISTRATION_FAILED.toStdString())));
            return;
        }

        m_authState.set(AuthState(Authorized()));
    },
                          [this](auto, auto)
    {
        const auto REGISTRATION_FAILED = muse::qtrc("appshell/gettingstarted", "Registration failed. Please try again.");
        m_authState.set(AuthState(NotAuthorized(REGISTRATION_FAILED.toStdString())));
    }, AudiocomTrace::ignore);
}

void Au3CloudService::signInWithPassword(const std::string& email, const std::string& password)
{
    m_authState.set(Authorizing());

    auto& oauthService = audacity::cloud::audiocom::GetOAuthService();
    oauthService.Authorize(email, password,
                           [this](auto token)
    {
        if (token.empty()) {
            const auto INCORRECT_EMAIL_OR_PASSWORD = muse::qtrc("appshell/gettingstarted",
                                                                "Incorrect email or password. Please try again.");
            m_authState.set(AuthState(NotAuthorized(INCORRECT_EMAIL_OR_PASSWORD.toStdString())));
            return;
        }

        m_authState.set(AuthState(Authorized()));
    },
                           [this](auto, auto)
    {
        const auto INCORRECT_EMAIL_OR_PASSWORD = muse::qtrc("appshell/gettingstarted", "Incorrect email or password. Please try again.");
        m_authState.set(AuthState(NotAuthorized(INCORRECT_EMAIL_OR_PASSWORD.toStdString())));
    }, AudiocomTrace::ignore);
}

void Au3CloudService::signInWithSocial(const std::string& provider)
{
    m_authState.set(AuthState(Authorizing()));
    interactive()->openUrl(buildOAuthRequestURL(provider));
}

void Au3CloudService::signOut()
{
    auto& oauthService = audacity::cloud::audiocom::GetOAuthService();
    oauthService.UnlinkAccount(AudiocomTrace::ignore);
}

muse::ValCh<AuthState> Au3CloudService::authState() const
{
    return m_authState;
}

const AccountInfo& Au3CloudService::accountInfo() const
{
    return m_accountInfo;
}

void Au3CloudService::setSendAnonymousUsageInfo(bool allow)
{
    SendAnonymousUsageInfo->Write(allow);
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
