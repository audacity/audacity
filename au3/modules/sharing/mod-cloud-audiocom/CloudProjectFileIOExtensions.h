/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectFileIOExtensions.cpp

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <functional>

#include "sync/ProjectUploadOperation.h"

#include "GlobalVariable.h"

class AudacityProject;

namespace audacity::cloud::audiocom::sync {
struct OnCloseHook : GlobalHook<OnCloseHook, bool(AudacityProject&)>
{
};

struct CreateSnapshotResponse;
using CreateSnapshotCallback = std::function<void (const CreateSnapshotResponse&)>;

void SaveToCloud(
    AudacityProject& project, UploadMode mode, CreateSnapshotCallback snapshotCallback = {});

bool ResaveLocally(AudacityProject& project);
} // namespace audacity::cloud::audiocom::sync
