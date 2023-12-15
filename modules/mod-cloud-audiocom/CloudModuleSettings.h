/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ModulePrefs.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "Prefs.h"

namespace cloud::audiocom::sync
{
extern BoolSetting DoNotShowCloudSyncDialog;
extern BoolSetting SaveToCloudByDefault;
extern BoolSetting MixdownDialogShown;
extern IntSetting MixdownGenerationFrequency;
} // namespace cloud::audiocom::sync
