/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SyncInBackroundDialog.cpp

  Dmitry Vedenko

**********************************************************************/

#include "SyncInBackroundDialog.h"

namespace audacity::cloud::audiocom::sync
{
SyncInBackroundDialog::SyncInBackroundDialog(const AudacityProject* project)
    : AudioComDialogBase { project,
                           DialogIdentifier { "SyncInBackroundDialog" } }
{
   AddTitle(XO("Mixdown complete!"));

   AddParagraph(XO(
      "Your project is being saved and uploaded in the background. You can check on the status in the bottom right corner."));

   AddParagraph(XO("You can continue working on the project in the meantime."));

   AddParagraph(XO(
      "You can manage this file form your uploaded projects page on audio.com"));

   AddButton(ViewOnlineIdentifier(), XO("View online"));
   AddButton(OkIdentifier(), XO("OK"), DefaultButton | EscButton);
}

DialogButtonIdentifier SyncInBackroundDialog::OkIdentifier()
{
   return { L"ok" };
}

DialogButtonIdentifier SyncInBackroundDialog::ViewOnlineIdentifier()
{
   return { L"view_online" };
}

} // namespace audacity::cloud::audiocom::sync
