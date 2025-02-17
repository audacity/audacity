/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  LinkAccountDialog.cpp

  Dmitry Vedenko

**********************************************************************/
#include "LinkAccountDialog.h"

#include <wx/button.h>
#include <wx/font.h>
#include <wx/stattext.h>

#include "CodeConversions.h"
#include "ServiceConfig.h"

#include "ShuttleGui.h"

#include "HelpSystem.h"

namespace audacity::cloud::audiocom::sync {
LinkAccountDialog::LinkAccountDialog(
    const AudacityProject* project, const TranslatableString& alternativeButtonText)
    : AudioComDialogBase{project}
{
    AddTitle(XO("You are not signed in"));
    AddParagraph(XO("Log in to audio.com to proceed."));
    AddButton(
        CancelButtonIdentifier(), XO("Cancel"), AudioComDialogBase::EscButton);

    if (!alternativeButtonText.empty()) {
        AddButton(AlternativeButtonIdentifier(), alternativeButtonText);
    }

    AddButton(
        SignInButtonIdentifier(), XO("Sign in"), AudioComDialogBase::DefaultButton);
}

DialogButtonIdentifier LinkAccountDialog::AlternativeButtonIdentifier()
{
    return { L"alternative" };
}

DialogButtonIdentifier LinkAccountDialog::SignInButtonIdentifier()
{
    return { L"signin" };
}
} // namespace audacity::cloud::audiocom::sync
