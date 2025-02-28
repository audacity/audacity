/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectLimitDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "AudioComDialogBase.h"

namespace audacity::cloud::audiocom::sync {
class ProjectLimitDialog final : public AudioComDialogBase
{
public:
    ProjectLimitDialog(const AudacityProject* project);

    static DialogButtonIdentifier SaveLocallyButtonIdentifier();
    static DialogButtonIdentifier VisitAudioComIdentifier();
};
} // namespace audacity::cloud::audiocom::sync
