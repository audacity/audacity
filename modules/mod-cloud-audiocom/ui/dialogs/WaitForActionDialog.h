/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaitForActionDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "AudioComDialogBase.h"

namespace cloud::audiocom::sync
{
class WaitForActionDialog final : public AudioComDialogBase
{
public:
   WaitForActionDialog(
      const AudacityProject* project,
      const TranslatableString& message = XO("Waiting for audio.com"),
      bool retryButton = false);

private:
   bool HasSeparator() const override;
};
} // namespace cloud::audiocom::sync
