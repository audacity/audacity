/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AuthorizationHandler.cpp

  Dmitry Vedenko

**********************************************************************/
#include "AuthorizationHandler.h"

#include <cassert>

#include "OAuthService.h"

#include "LinkFailedDialog.h"
#include "LinkSucceededDialog.h"

namespace cloud::audiocom
{
namespace
{
AuthorizationHandler handler;
}

AuthorizationHandler& GetAuthorizationHandler()
{
   return handler;
}

AuthorizationHandler::AuthorizationHandler()
    : mAuthStateChangedSubscription(GetOAuthService().Subscribe(
         [this](const auto& message) { OnAuthStateChanged(message); }))
{
}

void AuthorizationHandler::PushSuppressDialogs()
{
   ++mSuppressed;
}

void AuthorizationHandler::PopSuppressDialogs()
{
   assert(mSuppressed > 0);

   if (mSuppressed > 0)
      --mSuppressed;
}

void AuthorizationHandler::OnAuthStateChanged(
   const AuthStateChangedMessage& message)
{
   if (mSuppressed > 0)
      return;

   if (!message.errorMessage.empty())
   {
      LinkFailedDialog dialog { nullptr };
      dialog.ShowModal();
   }
   else if (message.authorised)
   {
      LinkSucceededDialog dialog { nullptr };
      dialog.ShowModal();
   }
}
} // namespace cloud::audiocom
