/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  UnsyncedProjectDialog.cpp

  Dmitry Vedenko

**********************************************************************/

#include "UnsyncedProjectDialog.h"

namespace audacity::cloud::audiocom::sync {
UnsyncedProjectDialog::UnsyncedProjectDialog(
    const AudacityProject* project, bool hasValidSnapshot)
    : AudioComDialogBase{project}
{
    AddTitle(XO("Cloud project incomplete"));

    if (hasValidSnapshot) {
        AddParagraph(XO(
                         "The latest version of this project was not fully uploaded to audio.com. You can load the last complete version instead."));
        AddButton(
            CancelButtonIdentifier(), XO("Cancel"), DefaultButton | EscButton);
        AddButton(VisitAudioComButtonIdentifier(), XO("Visit audio.com"));
        AddButton(LoadLatestButtonIdentifier(), XO("Load latest"));
    } else {
        AddParagraph(XO(
                         "No version of this project has been fully uploaded to audio.com. It cannot be loaded."));
        AddButton(VisitAudioComButtonIdentifier(), XO("Visit audio.com"));
        AddButton(
            CancelButtonIdentifier(), XO("OK"), DefaultButton | EscButton);
    }
}

DialogButtonIdentifier UnsyncedProjectDialog::VisitAudioComButtonIdentifier()
{
    return { "visit_audiocom" };
}

DialogButtonIdentifier UnsyncedProjectDialog::LoadLatestButtonIdentifier()
{
    return { "load_latest" };
}
} // namespace audacity::cloud::audiocom::sync
