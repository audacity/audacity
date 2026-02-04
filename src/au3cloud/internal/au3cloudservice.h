/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "au3-utility/Observer.h"

#include "framework/global/async/asyncable.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/iinteractive.h"
#include "au3cloud/iauthorization.h"
#include "au3cloud/iusageinfo.h"

#include "au3cloud/cloudtypes.h"
#include "oauthhttpserverreplyhandler.h"

namespace au::au3cloud {
class Au3CloudService : public QObject, public muse::async::Asyncable, public IAuthorization, public IUsageInfo, public muse::Injectable
{
    Q_OBJECT

    muse::Inject<muse::IInteractive> interactive = { this };

public:
    Au3CloudService(const muse::modularity::ContextPtr& ctx)
        : muse::Injectable(ctx) {}

    void init();

    void registerWithPassword(const std::string& email, const std::string& password) override;
    void signInWithPassword(const std::string& email, const std::string& password) override;
    void signInWithSocial(const std::string& provider) override;
    void signOut() override;
    const AccountInfo& accountInfo() const override;
    muse::ValCh<AuthState> authState() const override;

    bool getSendAnonymousUsageInfo() const override;
    void setSendAnonymousUsageInfo(bool allow) override;

private:
    std::string buildOAuthRequestURL(const std::string& provider);

    Observer::Subscription m_authSubscription;
    Observer::Subscription m_urlRegisterSubscription;
    Observer::Subscription m_userDataSubscription;

    muse::ValCh<AuthState> m_authState;

    OAuthHttpServerReplyHandler* m_replyHandler;

    AccountInfo m_accountInfo;
};
}
