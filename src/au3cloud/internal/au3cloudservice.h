/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "au3-utility/Observer.h"

#include "framework/global/async/asyncable.h"
#include "framework/global/async/notification.h"

#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iplatforminteractive.h"

#include "usageinfo/iusageinfo.h"

#include "au3cloud/iauthorization.h"
#include "au3cloud/cloudtypes.h"
#include "oauthhttpserverreplyhandler.h"

namespace au::au3cloud {
class Au3CloudService : public QObject, public muse::async::Asyncable, public IAuthorization
{
    Q_OBJECT

    muse::GlobalInject<muse::IPlatformInteractive> platformInteractive;
    muse::GlobalInject<usageinfo::IUsageInfo> usageInfo;

public:
    void init();

    void registerWithPassword(const std::string& email, const std::string& password) override;
    void signInWithPassword(const std::string& email, const std::string& password) override;
    void signInWithSocial(const std::string& provider) override;
    void signOut() override;

    const AccountInfo& accountInfo() const override;
    muse::async::Notification accountInfoChanged() const override;

    muse::ValCh<AuthState> authState() const override;
    bool isAuthorized() const override;

private:
    std::string buildOAuthRequestURL(const std::string& provider);
    void syncUsageInfoPrefs();

    Observer::Subscription m_authSubscription;
    Observer::Subscription m_urlRegisterSubscription;
    Observer::Subscription m_userDataSubscription;

    muse::ValCh<AuthState> m_authState;

    OAuthHttpServerReplyHandler* m_replyHandler;

    AccountInfo m_accountInfo;
    muse::async::Notification m_accountInfoChanged;
};
}
