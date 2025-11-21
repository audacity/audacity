/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudLibrarySettings.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "Prefs.h"

namespace audacity::cloud::audiocom {
CLOUD_AUDIOCOM_API extern StringSetting CloudProjectsSavePath;
CLOUD_AUDIOCOM_API extern IntSetting DaysToKeepFiles;
} // namespace audacity::cloud::audiocom
