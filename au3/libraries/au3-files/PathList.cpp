/**********************************************************************

  Audacity: A Digital Audio Editor

  @file PathList.cpp

  Paul Licameli split from AudacityApp.cpp
  Rewritten to use PlatformCompatibility (Qt-free)

**********************************************************************/

#include "PathList.h"

#include "PlatformCompatibility.h"
#include "FileNames.h"
#include "TempDirectory.h"

#if defined(__linux__)
#include <dlfcn.h>
#endif

namespace PC = PlatformCompatibility;

namespace {

#if defined(__linux__)
// Get path to the library containing this code (for plugin search paths)
std::string getLibraryPath()
{
    Dl_info info;
    if (dladdr(reinterpret_cast<const void*>(getLibraryPath), &info)) {
        return info.dli_fname;
    }
    return {};
}
#endif

// Normalize and add path to list if not already present
void addUniquePath(FilePaths& list, const std::string& path)
{
    if (path.empty()) {
        return;
    }

    std::string normalized = PC::NormalizePath(path);
    wxString wxpath = wxString::FromUTF8(normalized.c_str());

    // Check for duplicates
    for (const auto& existing : list) {
        if (existing == wxpath) {
            return;
        }
    }

    list.push_back(wxpath);
}

// Add multiple paths from a path-separator-delimited string
void addMultiplePaths(FilePaths& list, const std::string& pathString)
{
    for (const auto& p : PC::SplitSearchPath(pathString)) {
        addUniquePath(list, p);
    }
}

} // anonymous namespace

void FileNames::InitializePathList()
{
    FilePaths searchPaths;

    // Get key paths
    const std::string binDir = PC::GetExecutableDir();
    const std::string userAppData = PC::GetUserLocalDataDir();
    const std::string home = PC::GetHomeDir();

#if defined(__linux__)
    // Linux: check for portable installation
    std::string installPrefix;
    const std::string portablePrefix = PC::GetParentDir(binDir);

    if (PC::DirectoryExists(portablePrefix + "/share/audacity")) {
        installPrefix = portablePrefix;
    } else {
#ifdef INSTALL_PREFIX
        installPrefix = INSTALL_PREFIX;
#else
        installPrefix = "/usr";
#endif
    }

    // Add AUDACITY_PATH environment variable paths
    addMultiplePaths(searchPaths, PC::GetEnvironmentVar("AUDACITY_PATH"));

    // Current working directory
    addUniquePath(searchPaths, PC::GetCurrentDir());

    // Executable directory and lib paths
    addUniquePath(searchPaths, binDir);
    addUniquePath(searchPaths, binDir + "/lib/audacity");

    // Parent lib directories (standard Unix layout: bin/../lib)
    std::string binParent = PC::GetParentDir(binDir);
    if (!binParent.empty()) {
        addUniquePath(searchPaths, binParent + "/lib/audacity");
        addUniquePath(searchPaths, binParent + "/lib");
    }

    // Library path (for finding modules relative to this library)
    std::string libPath = getLibraryPath();
    if (!libPath.empty()) {
        addUniquePath(searchPaths, PC::GetParentDir(libPath));
    }

    // User and system data directories
    addUniquePath(searchPaths, userAppData);
    addUniquePath(searchPaths, home + "/.audacity-files");
    addUniquePath(searchPaths, FileNames::ModulesDir().ToStdString());
    addUniquePath(searchPaths, installPrefix + "/share/audacity");
    addUniquePath(searchPaths, installPrefix + "/share/doc/audacity");
    addUniquePath(searchPaths, installPrefix + "/share/locale");
    addUniquePath(searchPaths, "./locale");

    // Set up temp directory
    std::string tmpDir = PC::GetEnvironmentVar("TMPDIR");
    if (tmpDir.empty()) {
        tmpDir = "/var/tmp";
    }
    TempDirectory::SetDefaultTempDir(
        wxString::FromUTF8((tmpDir + "/audacity-" + PC::GetUserId()).c_str()));

#elif defined(_WIN32)
    // Windows: executable directory and Languages subdirectory
    addUniquePath(searchPaths, binDir);
    addUniquePath(searchPaths, binDir + "\\Languages");

    // Set up temp directory in user local data
    PC::MakePath(userAppData);
    TempDirectory::SetDefaultTempDir(
        wxString::FromUTF8((userAppData + "\\SessionData").c_str()));

#elif defined(__APPLE__)
    // macOS: executable directory and bundle Resources
    addUniquePath(searchPaths, binDir);
    addUniquePath(searchPaths, binDir + "/../");
    addUniquePath(searchPaths, binDir + "/../Resources");

    // Set up temp directory in Application Support (persists across reboots)
    TempDirectory::SetDefaultTempDir(
        wxString::FromUTF8((home + "/Library/Application Support/audacity/SessionData").c_str()));

#endif

    FileNames::SetAudacityPathList(std::move(searchPaths));
}
