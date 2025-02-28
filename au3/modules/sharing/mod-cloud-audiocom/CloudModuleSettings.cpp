/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ModulePrefs.cpp

  Dmitry Vedenko

**********************************************************************/

#include "CloudModuleSettings.h"

namespace audacity::cloud::audiocom::sync {
EnumSetting<CloudLocationMode> SaveLocationMode {
    "/cloud/audiocom/SaveLocationMode",
    EnumValueSymbols { L"ask", L"local", L"cloud" },
    0,
    { CloudLocationMode::Ask, CloudLocationMode::Local,
      CloudLocationMode::Cloud }
};

EnumSetting<CloudLocationMode> ExportLocationMode {
    "/cloud/audiocom/ExportLocationMode",
    EnumValueSymbols { L"ask", L"local", L"cloud" },
    0,
    { CloudLocationMode::Ask, CloudLocationMode::Local,
      CloudLocationMode::Cloud }
};

BoolSetting MixdownDialogShown {
    "/cloud/audiocom/MixdownDialogShown", false
};
} // namespace audacity::cloud::audiocom::sync
