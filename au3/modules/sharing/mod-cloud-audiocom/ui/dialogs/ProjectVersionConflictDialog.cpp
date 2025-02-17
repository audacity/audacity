/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  NotCloudProjectDialog.cpp

  Dmitry Vedenko

**********************************************************************/

#include "ProjectVersionConflictDialog.h"

namespace audacity::cloud::audiocom::sync {
namespace {
struct DialogProperties final
{
    TranslatableString Title;
    TranslatableString Message;
    TranslatableString LocalButtonText;
    TranslatableString RemoteButtonText;
    bool HasCancelButton {};
};

const DialogProperties dialogProperties[] = {
    { XO("Project version conflict detected"),
      XO("There's a newer version of this Audacity project on Audio.com. Saving this project will replace it as the newest version instead."),
      XO("Save this project"), XO("Discard and open latest version"), false },
    { XO("Project version conflict detected"),
      XO(
          "Project contains unsaved changes. There's a newer version of this Audacity project on Audio.com. Discarding changes will open the latest version instead."),
      XO("Open local project"), XO("Discard and open latest version"), true },
    { XO("Cloud project conflict"),
      XO(
          "You are attempting to open a new active version of this project when there is already one open. Please select which version you wish to remain open."),
      XO("Keep currently open version"), XO("Open new version"), false },
};
} // namespace

ProjectVersionConflictDialog::ProjectVersionConflictDialog(
    const AudacityProject* project, ProjectVersionConflictDialogMode openMode)
    : AudioComDialogBase{project}
{
    const auto& properties = dialogProperties[static_cast<int>(openMode)];

    AddTitle(properties.Title);
    AddParagraph(properties.Message);

    if (properties.HasCancelButton) {
        AddButton(CancelButtonIdentifier(), XO("Cancel"), EscButton);
    }

    AddButton(
        UseLocalIdentifier(), properties.LocalButtonText,
        !properties.HasCancelButton ? EscButton : 0);

    AddButton(UseRemoteIdentifier(), properties.RemoteButtonText, DefaultButton);
}

DialogButtonIdentifier ProjectVersionConflictDialog::UseLocalIdentifier()
{
    return { L"local" };
}

DialogButtonIdentifier ProjectVersionConflictDialog::UseRemoteIdentifier()
{
    return { L"remote" };
}
} // namespace audacity::cloud::audiocom::sync
