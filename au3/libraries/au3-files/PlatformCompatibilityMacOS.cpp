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
#include <CoreServices/CoreServices.h>
#include <limits.h>
#include <mach-o/dyld.h>
#include <pwd.h>
#include <string>
#include <sys/types.h>
#include <unistd.h>

std::string PlatformCompatibility::GetUserDataDir()
{
    const char* homeDir = getenv("HOME");
    if (homeDir == nullptr) {
        homeDir = getpwuid(getuid())->pw_dir;
    }
    return std::string(homeDir) + "/Library/Application Support";
}

std::string PlatformCompatibility::GetUserLocalDataDir()
{
    // Same as GetUserDataDir for macOS
    return GetUserDataDir();
}

std::string PlatformCompatibility::GetDataDir()
{
    // For macOS, typically data directory could be the same as user data dir or
    // application bundle's SharedSupport
    char path[PATH_MAX];
    uint32_t size = sizeof(path);
    if (_NSGetExecutablePath(path, &size) == 0) {
        std::string executablePath(path);
        // Assuming data is in a sibling directory to the executable
        return executablePath.substr(0, executablePath.find_last_of("/"))
               + "/Contents/SharedSupport";
    }
    return "";
}

std::string PlatformCompatibility::GetPluginsDir()
{
    // Assuming plugins are in a sibling directory to the executable
    char path[PATH_MAX];
    uint32_t size = sizeof(path);
    if (_NSGetExecutablePath(path, &size) == 0) {
        std::string executablePath(path);
        // Assuming plugins are in a subdirectory named "Plugins"
        return executablePath.substr(0, executablePath.find_last_of("/"))
               + "/Contents/Plugins";
    }
    return "";
}

std::string PlatformCompatibility::GetDocumentsDir()
{
    const char* homeDir = getenv("HOME");
    if (homeDir == nullptr) {
        homeDir = getpwuid(getuid())->pw_dir;
    }
    return std::string(homeDir) + "/Documents";
}

std::string PlatformCompatibility::GetResourcesDir()
{
    char path[PATH_MAX];
    uint32_t size = sizeof(path);
    if (_NSGetExecutablePath(path, &size) == 0) {
        std::string executablePath(path);
        // Assuming resources are in a sibling directory to the executable
        return executablePath.substr(0, executablePath.find_last_of("/"))
               + "/Contents/Resources";
    }
    return "";
}

std::string PlatformCompatibility::GetExecutablePath()
{
    // TODO: Implement for macOS
    return "";
}

std::string PlatformCompatibility::GetTempDir()
{
    // TODO: Implement for macOS
    return "";
}

std::string PlatformCompatibility::GetHomeDir()
{
    // TODO: Implement for macOS
    return "";
}

FilePath PlatformCompatibility::GetLongFileName(const FilePath& shortFileName)
{
    // TODO: Implement for macOS (likely just return shortFileName as-is)
    return shortFileName;
}
