/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "au3-utility/Observer.h"

#include "framework/global/async/asyncable.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/iinteractive.h"
#include "au3cloud/iauthorization.h"
#include "au3cloud/iuserdata.h"
#include "au3cloud/iusageinfo.h"

namespace au::au3cloud {
class Au3CloudService : public muse::async::Asyncable, public IAuthorization, public IUserData, public IUsageInfo, public muse::Injectable
{
    muse::Inject<muse::IInteractive> interactive = { this };

public:
    Au3CloudService(const muse::modularity::ContextPtr& ctx)
        : muse::Injectable(ctx) {}

    void init();

    void registerWithPassword(const std::string& email, const std::string& password) override;
    void signInWithPassword(const std::string& email, const std::string& password) override;
    void signInWithSocial(const std::string& provider) override;
    void signOut() override;
    muse::ValCh<AuthState> authState() const override;

    std::string getAvatarPath() const override;
    std::string getDisplayName() const override;

    bool getSendAnonymousUsageInfo() const override;
    void setSendAnonymousUsageInfo(bool send) override;

private:
    Observer::Subscription m_authSubscription;
    Observer::Subscription m_urlRegisterSubscription;
    muse::ValCh<AuthState> m_authState;
};
}
