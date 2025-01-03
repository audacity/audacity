/**********************************************************************

  Audacity: A Digital Audio Editor

  PlatformCompatibility.cpp

  Markus Meyer

*******************************************************************//*!

\class PlatformCompatibility
\brief Filename Compatibility utilities.

\see FileNames

*//*******************************************************************/

#include "PlatformCompatibility.h"

std::string PlatformCompatibility::GetUserDataDir()
{
    return "";
}

std::string PlatformCompatibility::GetUserLocalDataDir()
{
    return "";
}

std::string PlatformCompatibility::GetDataDir()
{
    return "";
}

std::string PlatformCompatibility::GetPluginsDir()
{
    return "";
}

std::string PlatformCompatibility::GetDocumentsDir()
{
    return "";
}

std::string PlatformCompatibility::GetResourcesDir()
{
    return "";
}

std::string PlatformCompatibility::GetExecutablePath()
{
    return "";
}
