/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaitForActionDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "AudioComDialogBase.h"

namespace audacity::cloud::audiocom::sync {
class WaitForActionDialog final : public AudioComDialogBase
{
public:
    WaitForActionDialog(
        const AudacityProject* project, const TranslatableString& title   = XO("Waiting for audio.com"), const TranslatableString& message = XO(
            "An action on audio.com is required before you can continue. Once you are done with it, click Retry"),
        bool retryButton = true);

private:
    bool HasSeparator() const override;
};
} // namespace audacity::cloud::audiocom::sync
