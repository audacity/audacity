/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectVersionConflictDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "AudioComDialogBase.h"

namespace audacity::cloud::audiocom::sync {
enum class ProjectVersionConflictDialogMode
{
    Save,
    OpenDirty,
    OpenActive
};

class ProjectVersionConflictDialog final : public AudioComDialogBase
{
public:
    ProjectVersionConflictDialog(
        const AudacityProject* project, ProjectVersionConflictDialogMode openMode);

    static DialogButtonIdentifier UseLocalIdentifier();
    static DialogButtonIdentifier UseRemoteIdentifier();
};
} // namespace audacity::cloud::audiocom::sync
