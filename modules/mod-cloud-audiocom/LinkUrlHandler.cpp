/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  LinkUrlHandler.cpp

  Dmitry Vedenko

**********************************************************************/

#include "URLSchemesRegistry.h"

#include "ServiceConfig.h"
#include "OAuthService.h"
#include "CloudSyncService.h"
#include "CloudProjectUtils.h"

namespace
{
auto subscription = URLSchemesRegistry::Get().Subscribe(
   [](URLschemeHandlerMessage message)
   {
      using namespace audacity::cloud::audiocom;

      if (GetOAuthService().HandleLinkURI(message.url, {}))
         return;

      if (sync::HandleProjectLink(message.url))
         return;
   });
}
