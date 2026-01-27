/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/types/retval.h"

namespace au::au3cloud {
enum class AuthState : int {
    NotAuthorized = 0,
    Authorizing,
    Authorized
};

class IAuthorization : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IAuthorization)

public:
    virtual ~IAuthorization() = default;

    virtual void signInWithPassword(const std::string& email, const std::string& password) = 0;
    virtual void signOut() = 0;

    virtual muse::ValCh<AuthState> authState() const = 0;
};
}
