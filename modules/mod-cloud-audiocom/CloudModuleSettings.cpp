/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ModulePrefs.cpp

  Dmitry Vedenko

**********************************************************************/

#include "CloudModuleSettings.h"

namespace cloud::audiocom::sync
{
BoolSetting ShowCloudSyncDialog {
   "/cloud/audiocom/ShowCloudSyncDialog", true
};

BoolSetting SaveToCloudByDefault {
   "/cloud/audiocom/SaveToCloudByDefault", false
};

BoolSetting MixdownDialogShown {
   "/cloud/audiocom/MixdownDialogShown", false
};
} // namespace cloud::audiocom::sync
