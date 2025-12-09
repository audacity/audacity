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
#include <cassert>
#include <shlobj.h>

// At the moment, for this PoC, we assume Windows as platform. If the PoC is
// conclusive we'll write the code for the other platforms.
#include <windows.h>

#ifndef AUDACITY_VERSION
#   pragma error
#endif

namespace {
#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)

// TODO isn't there a way of making this constexpr?
auto GetAppName()
{
    const std::string v { TOSTRING(AUDACITY_VERSION) };
    if (v == "3") {
        return "audacity3";
    } else if (v == "4") {
        return "audacity4";
    }
    assert(false);
    return "audacity";
}

std::string GetUserDir(int dir)
{
    char path[MAX_PATH];
    if (SUCCEEDED(SHGetFolderPathA(NULL, dir, NULL, 0, path))) {
        return std::string(path);
    }
    return "";
}
} // namespace

std::string PlatformCompatibility::GetUserDataDir()
{
    char path[MAX_PATH];
    if (SUCCEEDED(SHGetFolderPathA(NULL, CSIDL_APPDATA, NULL, 0, path))) {
        return std::string(path) + "\\" + GetAppName();
    }
    return "";
}

std::string PlatformCompatibility::GetUserLocalDataDir()
{
    char path[MAX_PATH];
    if (SUCCEEDED(SHGetFolderPathA(NULL, CSIDL_LOCAL_APPDATA, NULL, 0, path))) {
        return std::string(path) + "\\" + GetAppName();
    }
    return "";
}

std::string PlatformCompatibility::GetDataDir()
{
    char path[MAX_PATH];
    if (SUCCEEDED(SHGetFolderPathA(NULL, CSIDL_PROGRAM_FILES, NULL, 0, path))) {
        return std::string(path) + "\\" + GetAppName();
    }
    return "";
}

std::string PlatformCompatibility::GetPluginsDir()
{
    char path[MAX_PATH];
    if (GetModuleFileNameA(NULL, path, MAX_PATH)) {
        std::string fullPath = std::string(path);
        return fullPath.substr(0, fullPath.find_last_of("\\/"));
    }
    return "";
}

std::string PlatformCompatibility::GetDocumentsDir()
{
    return GetUserDir(CSIDL_MYDOCUMENTS);
}

std::string PlatformCompatibility::GetResourcesDir()
{
    return GetDataDir();
}

std::string PlatformCompatibility::GetExecutablePath()
{
    // TODO: Implement for Windows
    return "";
}

std::string PlatformCompatibility::GetTempDir()
{
    // TODO: Implement for Windows
    return "";
}

std::string PlatformCompatibility::GetHomeDir()
{
    // TODO: Implement for Windows
    return "";
}

FilePath PlatformCompatibility::GetLongFileName(const FilePath& shortFileName)
{
    // TODO: Implement for Windows (convert short path to long path)
    return shortFileName;
}
