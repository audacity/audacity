/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SyncSucceededDialog.cpp

  Dmitry Vedenko

**********************************************************************/

#include "SyncSucceededDialog.h"

namespace audacity::cloud::audiocom::sync {
SyncSucceededDialog::SyncSucceededDialog(const AudacityProject* project)
    : AudioComDialogBase{project}
{
    AddTitle(XO("Success!"));
    AddParagraph(
        XO("All saved changes will now update to the cloud. You can manage this file from your uploaded projects page on audio.com"));

    AddButton(ViewOnlineIdentifier(), XO("Done"), DefaultButton);
}

DialogButtonIdentifier SyncSucceededDialog::ViewOnlineIdentifier()
{
    return { L"view_online" };
}
} // namespace audacity::cloud::audiocom::sync
