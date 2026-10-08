/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <functional>

#include "framework/global/types/ret.h"
#include "framework/global/io/path.h"

#include <string>

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/modularity/ioc.h"
#include "framework/global/types/retval.h"

#include "cloudtypes.h"
namespace au::au3cloud {
class IAuthorization : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(IAuthorization)

public:
    virtual ~IAuthorization() = default;

    virtual void registerWithPassword(const std::string& email, const std::string& password) = 0;
    virtual void signInWithPassword(const std::string& email, const std::string& password) = 0;
    virtual void signInWithSocial(const std::string& provider) = 0;
    virtual void signOut() = 0;

    virtual const AccountInfo& accountInfo() const = 0;
    virtual muse::async::Notification accountInfoChanged() const = 0;

    virtual muse::ValCh<AuthState> authState() const = 0;
    virtual bool isAuthorized() const = 0;

    virtual muse::Ret ensureAuthorized(const muse::modularity::ContextPtr& ctx, bool createAccountMode = false) = 0;
    //! Refreshes the access token, so that it lasts as long as possible, and
    //! writes it, readable by this user only, for a process started by this one
    //! to sign in with (IAu3CloudConfiguration::setAccessTokenFile) instead of
    //! refreshing the shared sign-in itself, which would invalidate this
    //! process's tokens. `onDone` runs on the main thread.
    virtual void writeAccessTokenFile(const muse::io::path_t& path, std::function<void(muse::Ret)> onDone) = 0;
};
}
