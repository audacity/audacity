/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ModulePrefs.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "Prefs.h"

namespace audacity::cloud::audiocom::sync {
enum class CloudLocationMode
{
    Ask,
    Local,
    Cloud,
};

extern EnumSetting<CloudLocationMode> SaveLocationMode;
extern EnumSetting<CloudLocationMode> ExportLocationMode;

extern BoolSetting MixdownDialogShown;
} // namespace audacity::cloud::audiocom::sync
