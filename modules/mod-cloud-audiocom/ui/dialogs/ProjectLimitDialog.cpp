/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectLimitDialog.cpp

  Dmitry Vedenko

**********************************************************************/
#include "ProjectLimitDialog.h"

namespace cloud::audiocom::sync
{
cloud::audiocom::sync::ProjectLimitDialog::ProjectLimitDialog(
   const AudacityProject* project)
    : AudioComDialogBase { project }
{
   AddTitle(XO("Your project storage limit has been reached."));
   AddParagraphWithLink(
      XO("You may need to remove older projects to make space available. For more options, visit %s"),
      "%s", "audio.com", "https://audio.com");
   AddParagraph(
      XO("You can also save this project locally to avoid losing changes."));
   AddButton(SaveLocallyButtonIdentifier(), XO("Save to computer"), EscButton);
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
} // namespace cloud::audiocom::sync
