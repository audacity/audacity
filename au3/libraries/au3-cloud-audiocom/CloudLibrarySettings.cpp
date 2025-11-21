/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudLibrarySettings.cpp

  Dmitry Vedenko

**********************************************************************/
#include "CloudLibrarySettings.h"

#include "au3-files/FileNames.h"
#include "au3-files/wxFileNameWrapper.h"

namespace audacity::cloud::audiocom {
StringSetting CloudProjectsSavePath {
    "/cloud/audiocom/CloudProjectsSavePath",
    []
    {
        wxFileNameWrapper path { FileNames::DataDir(), "" };
        path.AppendDir("CloudProjects");
        return path.GetPath();
    }
};

IntSetting DaysToKeepFiles {
    "/cloud/audiocom/DaysToKeepFiles", 30
};
} // namespace audacity::cloud::audiocom
