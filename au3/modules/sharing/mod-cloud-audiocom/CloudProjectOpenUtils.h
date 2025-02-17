/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectOpenUtils.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <functional>
#include <string_view>

class AudacityProject;

enum class AudiocomTrace;

namespace audacity::cloud::audiocom::sync {
AudacityProject* GetPotentialTarget();
AudacityProject* GetOpenedProject(std::string_view projectId);

AudacityProject* OpenProjectFromCloud(
    AudacityProject* potentialTarget, std::string_view projectId, std::string_view snapshotId, bool forceNew);

bool SyncCloudProject(
    AudacityProject & project, std::string_view path, AudiocomTrace,
    bool force = false);

bool HandleProjectLink(std::string_view link);

void ReopenProject(AudacityProject& project);
} // namespace audacity::cloud::audiocom::sync
