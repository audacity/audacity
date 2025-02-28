/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AuthorizationHandler.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <string>

#include "Observer.h"
#include "TranslatableString.h"

class AudacityProject;
enum class AudiocomTrace;

namespace audacity::cloud::audiocom {
struct AuthStateChangedMessage;

class AuthorizationHandler final
{
public:
    AuthorizationHandler();

    void PushSuppressDialogs();
    void PopSuppressDialogs();

private:
    void OnAuthStateChanged(const AuthStateChangedMessage& message);

    Observer::Subscription mAuthStateChangedSubscription;

    size_t mSuppressed {};
}; // class AuthorizationHandler

AuthorizationHandler& GetAuthorizationHandler();

struct AuthResult final
{
    enum class Status
    {
        Authorised,
        Cancelled,
        UseAlternative,
        Failure
    };

    Status Result { Status::Cancelled };
    std::string ErrorMessage;
};

AuthResult PerformBlockingAuth(
    AudacityProject* project, AudiocomTrace trace, const TranslatableString& alternativeActionLabel = {});
} // namespace audacity::cloud::audiocom
