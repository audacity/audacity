/**********************************************************************

  Audacity: A Digital Audio Editor

  @file PathList.cpp

  Paul Licameli split from AudacityApp.cpp

**********************************************************************/

#include "PathList.h"

#include "PlatformCompatibility.h"
#include "FileNames.h"
#include "TempDirectory.h"
#include <wx/stdpaths.h>
#include <wx/utils.h>

#if HAVE_DLFCN_H && !defined(DISABLE_DLADDR)
#  if __linux__ && !defined(_GNU_SOURCE)
#    define _GNU_SOURCE
#  endif
#  include <dlfcn.h>
#  define HAVE_GET_LIBRARY_PATH 1
namespace {
wxString GetLibraryPath()
{
    Dl_info info;
    // This is a GNU extension, but it's also supported on FreeBSD, OpenBSD, macOS and Solaris.
    if (dladdr(reinterpret_cast<const void*>(GetLibraryPath), &info)) {
        return info.dli_fname;
    }
    return {};
}
}
#endif

void FileNames::InitializePathList()
{
    const auto programPath = PlatformCompatibility::GetExecutablePath();

    //
    // Paths: set search path and temp dir path
    //
    FilePaths audacityPathList;
    auto& standardPaths = wxStandardPaths::Get();

#ifdef __WXGTK__
    const auto portablePrefix = wxPathOnly(wxPathOnly(programPath));

    // Make sure install prefix is set so wxStandardPath resolves paths properly
    if (wxDirExists(portablePrefix + L"/share/audacity")) {
        // use prefix relative to executable location to make Audacity portable
        standardPaths.SetInstallPrefix(portablePrefix);
    } else {
        // fallback to hard-coded prefix set during configuration
        standardPaths.SetInstallPrefix(wxT(INSTALL_PREFIX));
    }
    wxString installPrefix = standardPaths.GetInstallPrefix();

    /* Search path (for plug-ins, translations etc) is (in this order):
       * The AUDACITY_PATH environment variable
       * The current directory
       * The user's "~/.audacity-data" or "Portable Settings" directory
       * The user's "~/.audacity-files" directory
       * The "share" and "share/doc" directories in their install path */
    wxString home = wxGetHomeDir();

    wxString envTempDir = wxGetenv(wxT("TMPDIR"));
    if (!envTempDir.empty()) {
        /* On Unix systems, the environment variable TMPDIR may point to
           an unusual path when /tmp and /var/tmp are not desirable. */
        TempDirectory::SetDefaultTempDir(wxString::Format(
                                             wxT("%s/audacity-%s"), envTempDir, wxGetUserId()));
    } else {
        /* On Unix systems, the default temp dir is in /var/tmp. */
        TempDirectory::SetDefaultTempDir(wxString::Format(
                                             wxT("/var/tmp/audacity-%s"), wxGetUserId()));
    }

    wxString pathVar = wxGetenv(wxT("AUDACITY_PATH"));

    if (!pathVar.empty()) {
        FileNames::AddMultiPathsToPathList(pathVar, audacityPathList);
    }
    FileNames::AddUniquePathToPathList(::wxGetCwd(), audacityPathList);

    const auto progPath = wxPathOnly(programPath);

    FileNames::AddUniquePathToPathList(progPath, audacityPathList);
    // Add the path to modules:
    FileNames::AddUniquePathToPathList(progPath + L"/lib/audacity", audacityPathList);

#if !defined(__WXMSW__)
    // On Unix systems, the common directory structure is
    // .../bin
    // .../lib
    const wxString progParentPath = wxPathOnly(progPath);

    if (!progParentPath.IsEmpty()) {
        FileNames::AddUniquePathToPathList(progParentPath + L"/lib/audacity", audacityPathList);
        FileNames::AddUniquePathToPathList(progParentPath + L"/lib", audacityPathList);
    }

#if HAVE_GET_LIBRARY_PATH
    const wxString thisLibPath = GetLibraryPath();
    if (!thisLibPath.IsEmpty()) {
        FileNames::AddUniquePathToPathList(wxPathOnly(thisLibPath), audacityPathList);
    }
#endif
#endif

    FileNames::AddUniquePathToPathList(FileNames::DataDir(), audacityPathList);

#ifdef AUDACITY_NAME
    FileNames::AddUniquePathToPathList(wxString::Format(wxT("%s/.%s-files"),
                                                        home, wxT(AUDACITY_NAME)),
                                       audacityPathList);
    FileNames::AddUniquePathToPathList(FileNames::ModulesDir(),
                                       audacityPathList);
    FileNames::AddUniquePathToPathList(wxString::Format(installPrefix + L"/share/%s", wxT(AUDACITY_NAME)),
                                       audacityPathList);
    FileNames::AddUniquePathToPathList(wxString::Format(installPrefix + L"/share/doc/%s", wxT(AUDACITY_NAME)),
                                       audacityPathList);
#else //AUDACITY_NAME
    FileNames::AddUniquePathToPathList(wxString::Format(wxT("%s/.audacity-files"),
                                                        home),
                                       audacityPathList);
    FileNames::AddUniquePathToPathList(FileNames::ModulesDir(),
                                       audacityPathList);
    FileNames::AddUniquePathToPathList(installPrefix + L"/share/audacity",
                                       audacityPathList);
    FileNames::AddUniquePathToPathList(installPrefix + L"/share/doc/audacity",
                                       audacityPathList);
#endif //AUDACITY_NAME

    FileNames::AddUniquePathToPathList(installPrefix + L"/share/locale",
                                       audacityPathList);

    FileNames::AddUniquePathToPathList(wxString::Format(wxT("./locale")),
                                       audacityPathList);

#endif //__WXGTK__

// JKC Bug 1220: Use path based on home directory on WXMAC
#ifdef __WXMAC__
    wxFileName tmpFile;
    tmpFile.AssignHomeDir();
    wxString tmpDirLoc = tmpFile.GetPath(wxPATH_GET_VOLUME);
#else
    wxFileName tmpFile;
    tmpFile.AssignTempFileName(wxT("nn"));
    wxString tmpDirLoc = tmpFile.GetPath(wxPATH_GET_VOLUME);
    ::wxRemoveFile(tmpFile.GetFullPath());
#endif

    // On Mac and Windows systems, use the directory which contains Audacity.
#ifdef __WXMSW__
    // On Windows, the path to the Audacity program is programPath
    const auto progPath = wxPathOnly(programPath);
    FileNames::AddUniquePathToPathList(progPath, audacityPathList);
    FileNames::AddUniquePathToPathList(progPath + wxT("\\Languages"), audacityPathList);

    // See bug #1271 for explanation of location
    tmpDirLoc = FileNames::MkDir(wxStandardPaths::Get().GetUserLocalDataDir());
    TempDirectory::SetDefaultTempDir(wxString::Format(
                                         wxT("%s\\SessionData"), tmpDirLoc));
#endif //__WXWSW__

#ifdef __WXMAC__
    // On Mac OS X, the path to the Audacity program is programPath
    const auto progPath = wxPathOnly(programPath);

    FileNames::AddUniquePathToPathList(progPath, audacityPathList);
    // If Audacity is a "bundle" package, then the root directory is
    // the great-great-grandparent of the directory containing the executable.
    //FileNames::AddUniquePathToPathList(progPath + wxT("/../../../"), audacityPathList);

    // These allow for searching the "bundle"
    FileNames::AddUniquePathToPathList(
        progPath + wxT("/../"), audacityPathList);
    FileNames::AddUniquePathToPathList(
        progPath + wxT("/../Resources"), audacityPathList);

    // JKC Bug 1220: Using an actual temp directory for session data on Mac was
    // wrong because it would get cleared out on a reboot.
    TempDirectory::SetDefaultTempDir(wxString::Format(
                                         wxT("%s/Library/Application Support/audacity/SessionData"), tmpDirLoc));

    //TempDirectory::SetDefaultTempDir( wxString::Format(
    //   wxT("%s/audacity-%s"),
    //   tmpDirLoc,
    //   wxGetUserId() ) );
#endif //__WXMAC__

    FileNames::SetAudacityPathList(std::move(audacityPathList));
}
