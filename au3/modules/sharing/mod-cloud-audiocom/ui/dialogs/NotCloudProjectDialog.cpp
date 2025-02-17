/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectVersionConflictDialog.cpp

  Dmitry Vedenko

**********************************************************************/

#include "NotCloudProjectDialog.h"

namespace audacity::cloud::audiocom::sync {
NotCloudProjectDialog::NotCloudProjectDialog(const AudacityProject* project)
    : AudioComDialogBase{project}
{
    AddTitle(XO("This project is no longer saved to the Cloud"));
    AddParagraph(XO(
                     "This project was removed from audio.com and therefore cannot be saved at this time. "));
    AddParagraph(
        XO("You can either save it to the Cloud as a new project, or save it to your computer."));

    AddButton(SaveLocallyIdentifier(), XO("Save to computer"), EscButton);
    AddButton(
        SaveRemotelyIdentifier(), XO("Save to Cloud"), DefaultButton);
}

DialogButtonIdentifier NotCloudProjectDialog::SaveLocallyIdentifier()
{
    return { L"locally" };
}

DialogButtonIdentifier NotCloudProjectDialog::SaveRemotelyIdentifier()
{
    return { L"remotely" };
}
} // namespace audacity::cloud::audiocom::sync
