/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SyncSuccessDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "AudioComDialogBase.h"

namespace audacity::cloud::audiocom::sync
{
class SyncSuccessDialog final : public AudioComDialogBase
{
public:
   explicit SyncSuccessDialog(const AudacityProject* project);

   static DialogButtonIdentifier DoneIdentifier();
   static DialogButtonIdentifier ViewOnlineIdentifier();
};
} // namespace audacity::cloud::audiocom::sync
