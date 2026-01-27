/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "framework/global/async/asyncable.h"

#include "au3-utility/Observer.h"

#include "au3cloud/iauthorization.h"
#include "au3cloud/iuserdata.h"

namespace au::au3cloud {
class Au3CloudService : public muse::async::Asyncable, public IAuthorization, public IUserData
{
public:
    void init();

    void signInWithPassword(const std::string& email, const std::string& password) override;
    void signOut() override;
    muse::ValCh<AuthState> authState() const override;

    std::string getAvatarPath() const override;
    std::string getDisplayName() const override;

private:
    Observer::Subscription m_authSubscription;
    muse::ValCh<AuthState> m_authState;
};
}
