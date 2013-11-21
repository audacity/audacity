/**********************************************************************

  Audacity: A Digital Audio Editor

  FileNames.cpp

  James Crook

********************************************************************//**

\class FileNames
\brief Provides Static functions to yield filenames.

This class helps us with setting a base path, and makes it easier 
for us to keep track of the different kinds of files we read and write 
from.

JKC: In time I plan to add all file names and file extensions
used throughout Audacity into this one place.

*//********************************************************************/

#include "Audacity.h"

#include <wx/defs.h>
#include <wx/filename.h>
#include <wx/intl.h>
#include <wx/stdpaths.h>
#include "Prefs.h"
#include "FileNames.h"
#include "Internat.h"
#include "PlatformCompatibility.h"

#if defined(__WXMAC__) || defined(__WXGTK__)
#include <dlfcn.h>
#endif

#if defined(__WXMSW__)
#include <windows.h>
#endif

static wxString gDataDir;

wxString FileNames::MkDir(const wxString &Str)
{
   // Behaviour of wxFileName::DirExists() and wxFileName::MkDir() has
   // changed between wx2.6 and wx2.8, so we use static functions instead.
   if (!wxFileName::DirExists(Str))
      wxFileName::Mkdir(Str, 511, wxPATH_MKDIR_FULL);

   return Str;
}

/// Returns the directory used for temp files.
/// \todo put a counter in here to see if it gets used a lot.
/// if it does, then maybe we should cache the path name 
/// each time.
wxString FileNames::TempDir()
{
   return FileNames::MkDir(gPrefs->Read(wxT("/Directories/TempDir"), wxT("")));
}

// originally an ExportMultiple method. Append suffix if newName appears in otherNames.
void FileNames::MakeNameUnique(wxArrayString &otherNames, wxFileName &newName)
{
   if (otherNames.Index(newName.GetFullName(), false) >= 0) {
      int i=2;
      wxString orig = newName.GetName();
      do {
         newName.SetName(wxString::Format(wxT("%s-%d"), orig.c_str(), i));
         i++;
      } while (otherNames.Index(newName.GetFullName(), false) >= 0);
   }
   otherNames.Add(newName.GetFullName());
}



//
// Audacity user data directories
wxString FileNames::AutoSaveDir()
{
   wxFileName autoSaveDir(FileNames::DataDir(), wxT("AutoSave"));
   return FileNames::MkDir(autoSaveDir.GetFullPath());
}

wxString FileNames::DataDir()
{
   // LLL:  Wouldn't you know that as of WX 2.6.2, there is a conflict
   //       between wxStandardPaths and wxConfig under Linux.  The latter
   //       creates a normal file as "$HOME/.audacity", while the former
   //       expects the ".audacity" portion to be a directory.
   if (gDataDir.IsEmpty())
   {
      // If there is a directory "Portable Settings" relative to the
      // executable's EXE file, the prefs are stored in there, otherwise
      // the prefs are stored in the user data dir provided by the OS.
      wxFileName exePath(PlatformCompatibility::GetExecutablePath());
#if defined(__WXMAC__)
      // This removes (for instance) "Audacity.app/Contents/MacOSX"
      exePath.RemoveLastDir();
      exePath.RemoveLastDir();
      exePath.RemoveLastDir();
#endif
      wxFileName portablePrefsPath(exePath.GetPath(), wxT("Portable Settings"));
      
      if (::wxDirExists(portablePrefsPath.GetFullPath()))
      {
         // Use "Portable Settings" folder
         gDataDir = portablePrefsPath.GetFullPath();
      } else
      {
         // Use OS-provided user data dir folder
         wxString dataDir;
#if defined( __WXGTK__ )
         dataDir = wxStandardPaths::Get().GetUserDataDir() + wxT("-data");
#else
         dataDir = wxStandardPaths::Get().GetUserDataDir();
#endif

         gDataDir = FileNames::MkDir(dataDir);
      }
   }
   
   return gDataDir;
}

wxString FileNames::HtmlHelpDir()
{
#if defined(__WXMAC__)
   wxFileName exePath(PlatformCompatibility::GetExecutablePath());
      // This removes (for instance) "Audacity.app/Contents/MacOSX"
      exePath.RemoveLastDir();
      exePath.RemoveLastDir();
      exePath.RemoveLastDir();
   
   return wxFileName( exePath.GetPath()+wxT("/help/manual"), wxEmptyString ).GetFullPath();
#else
   //linux goes into /*prefix*/share/audacity/
   //windows goes into the dir containing the .exe
   wxString exeDir = wxStandardPaths::Get().GetDataDir();
   
   //for mac this puts us within the .app: Audacity.app/Contents/SharedSupport/
   return wxFileName( exeDir+wxT("/help/manual"), wxEmptyString ).GetFullPath();
#endif   
}

wxString FileNames::HtmlHelpIndexFile(bool quick)
{
   wxString htmlHelpIndex;
   if(quick)
      htmlHelpIndex = wxFileName( HtmlHelpDir(), wxT("quick_help.html") ).GetFullPath();
   else
      htmlHelpIndex = wxFileName( HtmlHelpDir(), wxT("index.html") ).GetFullPath();
   return htmlHelpIndex;
}

wxString FileNames::ChainDir()
{
   return FileNames::MkDir( wxFileName( DataDir(), wxT("Chains") ).GetFullPath() );
}

wxString FileNames::NRPDir()
{
   return FileNames::MkDir( wxFileName( DataDir(), wxT("NRP") ).GetFullPath() );
}

wxString FileNames::NRPFile()
{
   return wxFileName( NRPDir(), wxT("noisegate.nrp") ).GetFullPath();
}

wxString FileNames::PlugInDir()
{
   return FileNames::MkDir( wxFileName( DataDir(), wxT("Plug-Ins") ).GetFullPath() );
}

wxString FileNames::PluginsCache()
{
   return wxFileName( DataDir(), wxT("plugins.cfg") ).GetFullPath();
}

wxString FileNames::ThemeDir()
{
   return FileNames::MkDir( wxFileName( DataDir(), wxT("Theme") ).GetFullPath() );
}

wxString FileNames::ThemeComponentsDir()
{
   return FileNames::MkDir( wxFileName( ThemeDir(), wxT("Components") ).GetFullPath() );
}

wxString FileNames::ThemeCachePng()
{
   return wxFileName( ThemeDir(), wxT("ImageCache.png") ).GetFullPath();
}

wxString FileNames::ThemeCacheHtm()
{
   return wxFileName( ThemeDir(), wxT("ImageCache.htm") ).GetFullPath();
}

wxString FileNames::ThemeImageDefsAsCee()
{
   return wxFileName( ThemeDir(), wxT("ThemeImageDefsAsCee.h") ).GetFullPath();
}

wxString FileNames::ThemeCacheAsCee( )
{
   return wxFileName( ThemeDir(), wxT("ThemeAsCeeCode.h") ).GetFullPath();
}

wxString FileNames::ThemeComponent(const wxString &Str)
{
   return wxFileName( ThemeComponentsDir(), Str, wxT("png") ).GetFullPath();
}

//
// Returns the full path of program module (.exe, .dll, .so, .dylib) containing address
//
wxString FileNames::PathFromAddr(void *addr)
{
    wxFileName name;

#if defined(__WXMAC__) || defined(__WXGTK__)
   Dl_info info;
   if (dladdr(addr, &info)) {
      char realname[PLATFORM_MAX_PATH + 1];
      int len;
      name = LAT1CTOWX(info.dli_fname);
      len = readlink(OSINPUT(name.GetFullPath()), realname, PLATFORM_MAX_PATH);
      if (len > 0) {
         realname[len] = 0;
         name.SetFullName(LAT1CTOWX(realname));
      }
   }
#elif defined(__WXMSW__) && defined(_UNICODE)
   // The GetModuleHandlEx() function did not appear until Windows XP and
   // GetModuleFileName() did appear until Windows 2000, so we have to 
   // check for them at runtime.
   typedef BOOL (WINAPI *getmodulehandleex)(DWORD dwFlags, LPCWSTR lpModuleName, HMODULE* phModule);
   typedef DWORD (WINAPI *getmodulefilename)(HMODULE hModule, LPWCH lpFilename, DWORD nSize);
   getmodulehandleex gmhe =
      (getmodulehandleex) GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")),
                                         "GetModuleHandleExW");
   getmodulefilename gmfn =
      (getmodulefilename) GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")),
                                         "GetModuleFileNameW");

   if (gmhe != NULL && gmfn != NULL) {
      HMODULE module;
      if (gmhe(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
               (LPTSTR) addr,
               &module)) {
         TCHAR path[MAX_PATH];
         DWORD nSize;

         nSize = gmfn(module, path, MAX_PATH);
         if (nSize && nSize < MAX_PATH) {
            name = LAT1CTOWX(path);
         }
      }
   }
#endif

    return name.GetFullPath();
}
