/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectVersionConflictDialog.cpp

  Dmitry Vedenko

**********************************************************************/

#include "NotCloudProjectDialog.h"

namespace audacity::cloud::audiocom::sync
{
NotCloudProjectDialog::NotCloudProjectDialog(const AudacityProject* project)
    : AudioComDialogBase { project }
{
   AddTitle(XO("This project is no longer saved to the cloud"));
   AddParagraph(XO(
      "This project was removed from audio.com and therefore cannot be saved at this time. "));
   AddParagraph(
      XO("You can either upload this as a new project. Or, save it locally."));

   AddButton(SaveLocallyIdentifier(), XO("Save Locally"), EscButton);
   AddButton(
      SaveRemotelyIdentifier(), XO("Save Remotely"), DefaultButton);
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
