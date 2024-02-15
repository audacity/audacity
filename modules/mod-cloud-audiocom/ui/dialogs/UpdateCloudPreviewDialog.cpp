/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  UploadCancelledDialog.cpp

  Dmitry Vedenko

**********************************************************************/

#include "UpdateCloudPreviewDialog.h"

namespace cloud::audiocom::sync
{
UpdateCloudPreviewDialog::UpdateCloudPreviewDialog(const AudacityProject* project)
    : AudioComDialogBase { project, { L"UploadCloudPreviewDialog" } }
{
   AddTitle(XO("Update Cloud Preview?"));
   AddParagraph(XO("Your cloud audio preview may not match your latest project edits. Would you like to upload an up-to-date audio preview?"));
   AddParagraph(XO("You can do this anytime via the file menu."));
   AddButton(CancellButtonIdentifier(), XO("Cancel"), EscButton);
   AddButton(RenderPreviewIdentifier(), XO("Render audio preview"), DefaultButton);
}

DialogButtonIdentifier UpdateCloudPreviewDialog::RenderPreviewIdentifier()
{
   return { L"render" };
}

} // namespace cloud::audiocom::sync
