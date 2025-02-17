/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SyncInBackroundDialog.cpp

  Dmitry Vedenko

**********************************************************************/

#include "SyncInBackroundDialog.h"

namespace audacity::cloud::audiocom::sync {
SyncInBackroundDialog::SyncInBackroundDialog(const AudacityProject* project)
    : AudioComDialogBase{project,
                         DialogIdentifier { "SyncInBackroundDialog" }}
{
    AddTitle(XO("Syncing your project"));

    AddParagraph(XO(
                     "The project will sync in background while you work. You can check the sync status on the bottom right corner of Audacity at any time"));

    AddButton(OkIdentifier(), XO("Continue"), DefaultButton | EscButton);
}

DialogButtonIdentifier SyncInBackroundDialog::OkIdentifier()
{
    return { L"ok" };
}
} // namespace audacity::cloud::audiocom::sync
