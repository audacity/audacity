/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SyncSucceededDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "AudioComDialogBase.h"

namespace audacity::cloud::audiocom::sync {
class SyncSucceededDialog final : public AudioComDialogBase
{
public:
    explicit SyncSucceededDialog(const AudacityProject* project);

    static DialogButtonIdentifier DoneIdentifier();
    static DialogButtonIdentifier ViewOnlineIdentifier();
};
} // namespace audacity::cloud::audiocom::sync
