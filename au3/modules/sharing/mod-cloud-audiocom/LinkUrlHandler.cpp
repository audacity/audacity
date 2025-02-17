/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  LinkUrlHandler.cpp

  Dmitry Vedenko

**********************************************************************/

#include "URLSchemesRegistry.h"

#include "CloudProjectMixdownUtils.h"
#include "CloudProjectOpenUtils.h"
#include "ExportUtils.h"
#include "OAuthService.h"

namespace {
auto subscription = URLSchemesRegistry::Get().Subscribe(
    [](URLschemeHandlerMessage message)
{
    using namespace audacity::cloud::audiocom;

    if (GetOAuthService().HandleLinkURI(
            message.url,
            // TODO Is this correct?
            AudiocomTrace::ignore, {})) {
        return;
    }

    if (sync::HandleProjectLink(message.url)) {
        return;
    }

    if (sync::HandleMixdownLink(message.url)) {
        return;
    }
});
}
