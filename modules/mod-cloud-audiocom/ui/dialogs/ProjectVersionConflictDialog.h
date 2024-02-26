/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectVersionConflictDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "AudioComDialogBase.h"

namespace audacity::cloud::audiocom::sync
{
class ProjectVersionConflictDialog final : public AudioComDialogBase
{
public:
   ProjectVersionConflictDialog(const AudacityProject* project, bool onSave);

   static DialogButtonIdentifier UseLocalIdentifier();
   static DialogButtonIdentifier UseRemoteIdentifier();
};
} // namespace audacity::cloud::audiocom::sync
