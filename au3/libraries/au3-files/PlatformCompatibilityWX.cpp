/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  PlatformCompatibilityWX.cpp

  Matthieu Hodgkinson

  Requires a running wxApp to work. For another UI framework you'll need to use
  another source file.

**********************************************************************/

#include "PlatformCompatibility.h"
#include <wx/stdpaths.h>
#include <wx/utils.h>

std::string PlatformCompatibility::GetUserDataDir()
{
    return wxStandardPaths::Get().GetUserDataDir().ToStdString();
}

std::string PlatformCompatibility::GetUserLocalDataDir()
{
    return wxStandardPaths::Get().GetUserLocalDataDir().ToStdString();
}

std::string PlatformCompatibility::GetResourcesDir()
{
    return wxStandardPaths::Get().GetResourcesDir().ToStdString();
}

std::string PlatformCompatibility::GetDataDir()
{
    return wxStandardPaths::Get().GetDataDir().ToStdString();
}

std::string PlatformCompatibility::GetPluginsDir()
{
    return wxStandardPaths::Get().GetPluginsDir().ToStdString();
}

std::string PlatformCompatibility::GetDocumentsDir()
{
    return wxStandardPaths::Get().GetDocumentsDir().ToStdString();
}

std::string PlatformCompatibility::GetExecutablePath()
{
    return wxStandardPaths::Get().GetExecutablePath().ToStdString();
}

std::string PlatformCompatibility::GetTempDir()
{
    return wxStandardPaths::Get().GetTempDir().ToStdString();
}

std::string PlatformCompatibility::GetHomeDir()
{
    return wxGetHomeDir().ToStdString();
}
