/*
* Audacity: A Digital Audio Editor
*/
#include "authorizationstub.h"

using namespace au::au3cloud;

void AuthorizationStub::registerWithPassword(const std::string&, const std::string&)
{
}

void AuthorizationStub::signInWithPassword(const std::string&, const std::string&)
{
}

void AuthorizationStub::signInWithSocial(const std::string&)
{
}

void AuthorizationStub::signOut()
{
}

const AccountInfo& AuthorizationStub::accountInfo() const
{
    static const AccountInfo empty{};
    return empty;
}

muse::ValCh<AuthState> AuthorizationStub::authState() const
{
    muse::ValCh<AuthState> result;
    result.val = NotAuthorized{};
    return result;
}

bool AuthorizationStub::isAuthorized() const
{
    return false;
}

muse::Ret AuthorizationStub::ensureAuthorization()
{
    return muse::make_ret(muse::Ret::Code::NotSupported);
}

void AuthorizationStub::openSignInDialog()
{
}
