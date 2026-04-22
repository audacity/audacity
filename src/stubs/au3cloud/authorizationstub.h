/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "au3cloud/iauthorization.h"
#include "framework/global/modularity/ioc.h"

namespace au::au3cloud {
class AuthorizationStub : public IAuthorization
{
public:
    void registerWithPassword(const std::string& email, const std::string& password) override;
    void signInWithPassword(const std::string& email, const std::string& password) override;
    void signInWithSocial(const std::string& provider) override;
    void signOut() override;

    const AccountInfo& accountInfo() const override;

    muse::ValCh<AuthState> authState() const override;
    bool isAuthorized() const override;
};
}
