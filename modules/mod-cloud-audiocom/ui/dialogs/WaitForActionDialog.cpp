/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaitForActionDialog.cpp

  Dmitry Vedenko

**********************************************************************/
#include "WaitForActionDialog.h"

namespace cloud::audiocom::sync
{
WaitForActionDialog::WaitForActionDialog(
   const AudacityProject* project, const TranslatableString& message,
   bool retryButton)
    : AudioComDialogBase { project }

{
   AddParagraph(message);
   AddButton(
      CancellButtonIdentifier(), retryButton ? XO("Retry") : XO("Cancel"),
      EscButton | DefaultButton);
}

bool WaitForActionDialog::HasSeparator() const
{
   return false;
}
} // namespace cloud::audiocom::sync
