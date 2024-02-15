/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SyncSuccessDialog.cpp

  Dmitry Vedenko

**********************************************************************/

#include "SyncSuccessDialog.h"

namespace cloud::audiocom::sync
{
SyncSuccessDialog::SyncSuccessDialog(const AudacityProject* project)
    : AudioComDialogBase { project }
{
   AddTitle(XO("Success!"));
   AddParagraphWithLink(
      XO("All saved changes will now update to the cloud. You can manage this file from your uploaded projects page on %s"),
      "%s", "audio.com", "https://audio.com");

   AddButton(ViewOnlineIdentifier(), XO("View online"));
   AddButton(DoneIdentifier(), XO("Done"), DefaultButton | EscButton);
}

DialogButtonIdentifier SyncSuccessDialog::DoneIdentifier()
{
   return { L"done" };
}

DialogButtonIdentifier SyncSuccessDialog::ViewOnlineIdentifier()
{
   return { L"view_online" };
}

} // namespace cloud::audiocom::sync
