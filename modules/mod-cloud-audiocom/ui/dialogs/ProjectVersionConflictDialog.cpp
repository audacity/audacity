/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  NotCloudProjectDialog.cpp

  Dmitry Vedenko

**********************************************************************/

#include "ProjectVersionConflictDialog.h"

namespace cloud::audiocom::sync
{
namespace
{
const auto saveMessage = XO(
   "There's a newer version of this Audacity project on Audio.com. Saving this project will replace it as the newest version instead.");
const auto openMessage = XO(
   "Project contains unsaved changes. There's a newer version of this Audacity project on Audio.com. Discarding changes will open the latest version instead.");
const auto saveLocalMessage = XO("Save this project");
const auto openLocalMessage = XO("Open local project");
} // namespace

ProjectVersionConflictDialog::ProjectVersionConflictDialog(
   const AudacityProject* project, bool onSave)
    : AudioComDialogBase { project }
{
   AddTitle(XO("Project version conflict detected"));
   AddParagraph(onSave ? saveMessage : openMessage);

   if (!onSave)
      AddButton(CancellButtonIdentifier(), XO("Cancel"), EscButton);

   AddButton(
      UseLocalIdentifier(), onSave ? saveLocalMessage : openLocalMessage,
      onSave ? EscButton : 0);

   AddButton(
      UseRemoteIdentifier(), XO("Discard and open latest version"),
      DefaultButton);
}

DialogButtonIdentifier ProjectVersionConflictDialog::UseLocalIdentifier()
{
   return { L"local" };
}

DialogButtonIdentifier ProjectVersionConflictDialog::UseRemoteIdentifier()
{
   return { L"remote" };
}
} // namespace cloud::audiocom::sync
