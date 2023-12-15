/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ModulePrefs.cpp

  Dmitry Vedenko

**********************************************************************/

#include "CloudModuleSettings.h"

namespace cloud::audiocom::sync
{
BoolSetting DoNotShowCloudSyncDialog {
   "/cloud/audiocom/DoNotShowCloudSyncDialog", false
};

BoolSetting SaveToCloudByDefault {
   "/cloud/audiocom/SaveToCloudByDefault", false
};

IntSetting MixdownGenerationFrequency {
   "/cloud/audiocom/MixdownGenerationFrequency", 1
};
} // namespace cloud::audiocom::sync
