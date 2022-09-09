/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AuthorizationHandler.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "Observer.h"

namespace cloud::audiocom
{
struct AuthStateChangedMessage;

class AuthorizationHandler final
{
public:
   AuthorizationHandler();
   
   void PushSuppressDialogs();
   void PopSuppressDialogs();

private:
   void OnAuthStateChanged(const AuthStateChangedMessage& message);
   
   Observer::Subscription
      mAuthStateChangedSubscription;
   
   size_t mSuppressed {};
}; // class AuthorizationHandler

AuthorizationHandler& GetAuthorizationHandler();
} // namespace cloud::audiocom
