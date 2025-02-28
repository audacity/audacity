/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SyncInBackroundDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "AudioComDialogBase.h"

namespace audacity::cloud::audiocom::sync {
class SyncInBackroundDialog final : public AudioComDialogBase
{
public:
    explicit SyncInBackroundDialog(const AudacityProject* project);

    static DialogButtonIdentifier OkIdentifier();
};
} // namespace audacity::cloud::audiocom::sync
