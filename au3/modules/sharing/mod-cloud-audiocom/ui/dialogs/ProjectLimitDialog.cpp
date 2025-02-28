/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectLimitDialog.cpp

  Dmitry Vedenko

**********************************************************************/
#include "ProjectLimitDialog.h"

namespace audacity::cloud::audiocom::sync {
audacity::cloud::audiocom::sync::ProjectLimitDialog::ProjectLimitDialog(
    const AudacityProject* project)
    : AudioComDialogBase{project}
{
    AddTitle(XO("Your project storage limit has been reached."));
    AddParagraph(XO(
                     "You may need to remove older projects to make space available. For more options, visit audio.com"));
    AddParagraph(
        XO("You can also save this project locally to avoid losing changes."));
    AddButton(SaveLocallyButtonIdentifier(), XO("Save to computer"));
    AddButton(VisitAudioComIdentifier(), XO("Visit audio.com"), DefaultButton);
}

DialogButtonIdentifier ProjectLimitDialog::SaveLocallyButtonIdentifier()
{
    return { "save" };
}

DialogButtonIdentifier ProjectLimitDialog::VisitAudioComIdentifier()
{
    return { "visit" };
}
} // namespace audacity::cloud::audiocom::sync
