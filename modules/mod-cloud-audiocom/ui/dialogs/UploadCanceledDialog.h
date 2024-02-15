/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  UploadCancelledDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "AudioComDialogBase.h"

namespace cloud::audiocom::sync
{
class UploadCanceledDialog final : public AudioComDialogBase
{
public:
   explicit UploadCanceledDialog(const AudacityProject* project);

   static DialogButtonIdentifier OkButtonIdentifier();

private:
   bool HasSeparator() const override;
};
} // namespace cloud::audiocom::sync
