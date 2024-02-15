/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  UploadCanceledDialog.cpp

  Dmitry Vedenko

**********************************************************************/

#include "UploadCanceledDialog.h"

namespace cloud::audiocom::sync
{
UploadCanceledDialog::UploadCanceledDialog(const AudacityProject* project)
    : AudioComDialogBase { project }
{
   AddParagraph(XO("You have canceled this upload to audio.com"));
   AddButton(OkButtonIdentifier(), XO("OK"), DefaultButton | EscButton);
}

DialogButtonIdentifier UploadCanceledDialog::OkButtonIdentifier()
{
   return { L"ok" };
}

bool UploadCanceledDialog::HasSeparator() const
{
   return false;
}
} // namespace cloud::audiocom::sync
