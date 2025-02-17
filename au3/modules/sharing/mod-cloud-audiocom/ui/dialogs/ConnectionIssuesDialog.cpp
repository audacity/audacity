/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  UploadCancelledDialog.cpp

  Dmitry Vedenko

**********************************************************************/

#include "ConnectionIssuesDialog.h"

namespace audacity::cloud::audiocom::sync {
audacity::cloud::audiocom::sync::ConnectionIssuesDialog::ConnectionIssuesDialog(
    const AudacityProject* project)
    : AudioComDialogBase{project, { "ConnectionIssuesDialog" }}
{
    AddTitle(XO("We encountered an issue syncing your file"));
    AddParagraph(XO(
                     "Don't worry, your changes will be saved to a temporary location and will be synchronized to your cloud copy when your internet connection resumes."));
    AddButton(
        OkButtonIdentifier(), XO("OK"),
        DefaultButton | EscButton);
}

DialogButtonIdentifier ConnectionIssuesDialog::OkButtonIdentifier()
{
    return DialogButtonIdentifier();
}
} // namespace audacity::cloud::audiocom::sync
