/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/types/retval.h"

#include "cloudtypes.h"
namespace au::au3cloud {
class IAuthorization : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IAuthorization)

public:
    virtual ~IAuthorization() = default;

    virtual void registerWithPassword(const std::string& email, const std::string& password) = 0;
    virtual void signInWithPassword(const std::string& email, const std::string& password) = 0;
    virtual void signInWithSocial(const std::string& provider) = 0;
    virtual void signOut() = 0;

    virtual const AccountInfo& accountInfo() const = 0;

    virtual muse::ValCh<AuthState> authState() const = 0;
};
}
