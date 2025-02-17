/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  NotCloudProjectDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "AudioComDialogBase.h"

namespace audacity::cloud::audiocom::sync {
class NotCloudProjectDialog final : public AudioComDialogBase
{
public:
    explicit NotCloudProjectDialog(const AudacityProject* project);

    static DialogButtonIdentifier SaveLocallyIdentifier();
    static DialogButtonIdentifier SaveRemotelyIdentifier();
};
} // namespace audacity::cloud::audiocom::sync
