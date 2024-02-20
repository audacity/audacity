/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectUtils.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <string_view>
#include <memory>

#include "sync/AsynchronousOperation.h"

class AudacityProject;

namespace cloud::audiocom::sync
{
class LocalProjectSnapshot;

void OpenProjectFromCloud(
   AudacityProject* potentialTarget, std::string_view projectId,
   std::string_view snapshotId, bool forceNew);

bool SyncCloudProject(
   AudacityProject& project, std::string_view path, bool force = false);

void SaveToCloud(AudacityProject& project, UploadMode mode);

bool HandleProjectLink(std::string_view link);

void UploadMixdownForSnapshot(
   AudacityProject& project, std::shared_ptr<LocalProjectSnapshot> snapshot);

bool ResaveLocally(AudacityProject& project);

void ReopenProject(AudacityProject& project);
} // namespace cloud::audiocom::sync
