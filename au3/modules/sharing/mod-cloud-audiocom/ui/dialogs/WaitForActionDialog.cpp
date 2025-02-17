/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaitForActionDialog.cpp

  Dmitry Vedenko

**********************************************************************/
#include "WaitForActionDialog.h"

namespace audacity::cloud::audiocom::sync {
WaitForActionDialog::WaitForActionDialog(
    const AudacityProject* project, const TranslatableString& title,
    const TranslatableString& message, bool retryButton)
    : AudioComDialogBase{project}
{
    AddTitle(title);
    AddParagraph(message);
    AddButton(
        CancelButtonIdentifier(), retryButton ? XO("Retry") : XO("Cancel"),
        EscButton | DefaultButton);
}

bool WaitForActionDialog::HasSeparator() const
{
    return false;
}
} // namespace audacity::cloud::audiocom::sync
