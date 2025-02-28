/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  UnsyncedProjectDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "AudioComDialogBase.h"

namespace audacity::cloud::audiocom::sync {
class UnsyncedProjectDialog final : public AudioComDialogBase
{
public:
    UnsyncedProjectDialog(const AudacityProject* project, bool hasValidSnapshot);

    static DialogButtonIdentifier VisitAudioComButtonIdentifier();
    static DialogButtonIdentifier LoadLatestButtonIdentifier();
};
} // namespace audacity::cloud::audiocom::sync
