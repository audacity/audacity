/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectUtils.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <string_view>

class AudacityProject;

namespace cloud::audiocom::sync
{
void OpenProjectFromCloud(
   AudacityProject* potentialTarget, std::string_view projectId,
   std::string_view snapshotId = {});

bool HandleProjectLink(std::string_view link);
} // namespace cloud::audiocom::sync
