/**********************************************************************

  Audacity: A Digital Audio Editor

  PlatformCompatibility.cpp

  Markus Meyer

*******************************************************************//*!

\class PlatformCompatibility
\brief Filename Compatibility utilities.

\see FileNames

*//*******************************************************************/

#include "au3-files/PlatformCompatibility.h"

std::string PlatformCompatibility::GetUserDataDir()
{
    // TODO: Implement for Linux
    return "";
}

std::string PlatformCompatibility::GetUserLocalDataDir()
{
    // TODO: Implement for Linux
    return "";
}

std::string PlatformCompatibility::GetDataDir()
{
    // TODO: Implement for Linux
    return "";
}

std::string PlatformCompatibility::GetPluginsDir()
{
    // TODO: Implement for Linux
    return "";
}

std::string PlatformCompatibility::GetDocumentsDir()
{
    // TODO: Implement for Linux
    return "";
}

std::string PlatformCompatibility::GetResourcesDir()
{
    // TODO: Implement for Linux
    return "";
}

std::string PlatformCompatibility::GetExecutablePath()
{
    // TODO: Implement for Linux
    return "";
}

std::string PlatformCompatibility::GetTempDir()
{
    // TODO: Implement for Linux
    return "";
}

std::string PlatformCompatibility::GetHomeDir()
{
    // TODO: Implement for Linux
    return "";
}

FilePath PlatformCompatibility::GetLongFileName(const FilePath& shortFileName)
{
    // TODO: Implement for Linux (likely just return shortFileName as-is)
    return shortFileName;
}
