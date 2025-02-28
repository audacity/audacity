/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SyncFailedDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "AudioComDialogBase.h"

#include "sync/CloudSyncError.h"
#include "NetworkUtils.h"

namespace audacity::cloud::audiocom::sync {
class SyncFailedDialog final : public AudioComDialogBase
{
public:

    static void OnOpen(const CloudSyncError& error);
    static void OnSave(const CloudSyncError& error);

    static void OnOpen(const ResponseResult& error);
    static void OnSave(const ResponseResult& error);

private:
    SyncFailedDialog(
        const AudacityProject* project, const TranslatableString& message, const std::string& log, DialogMode dialogMode);
};
} // namespace audacity::cloud::audiocom::sync
