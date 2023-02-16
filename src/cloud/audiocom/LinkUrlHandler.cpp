/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  LinkUrlHandler.cpp

  Dmitry Vedenko

**********************************************************************/

#include "URLSchemesRegistry.h"

#include "ServiceConfig.h"
#include "OAuthService.h"

namespace
{
auto subscription = URLSchemesRegistry::Get().Subscribe(
   [](URLschemeHandlerMessage message)
   {
      cloud::audiocom::GetOAuthService().HandleLinkURI(message.url, {});
   });
}
