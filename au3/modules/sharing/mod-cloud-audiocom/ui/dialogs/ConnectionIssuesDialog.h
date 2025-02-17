/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ConnectionIssuesDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "AudioComDialogBase.h"

namespace audacity::cloud::audiocom::sync {
class ConnectionIssuesDialog final : public AudioComDialogBase
{
public:
    ConnectionIssuesDialog(const AudacityProject* project);

    static DialogButtonIdentifier OkButtonIdentifier();
};
} // namespace audacity::cloud::audiocom::sync
