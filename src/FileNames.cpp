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
#include "FileNames.h"

#include "Experimental.h"

#include "MemoryX.h"

#include <wx/defs.h>
#include <wx/filename.h>
#include <wx/intl.h>
#include <wx/stdpaths.h>
#include "Prefs.h"
#include "Internat.h"
#include "PlatformCompatibility.h"
#include "wxFileNameWrapper.h"
#include "../lib-src/FileDialog/FileDialog.h"

#if defined(__WXMAC__) || defined(__WXGTK__)
#include <dlfcn.h>
#endif

#if defined(__WXMSW__)
#include <windows.h>
#endif

static wxString gDataDir;

bool FileNames::CopyFile(
   const FilePath& file1, const FilePath& file2, bool overwrite)
{
#ifdef __WXMSW__

   // workaround not needed
   return wxCopyFile(file1, file2, overwrite);

#else
   // PRL:  Compensate for buggy wxCopyFile that returns false success,
   // which was a cause of case 4 in comment 10 of
   // http://bugzilla.audacityteam.org/show_bug.cgi?id=1759
   // Destination file was created, but was empty
   // Bug was introduced after wxWidgets 2.8.12 at commit
   // 0597e7f977c87d107e24bf3e95ebfa3d60efc249 of wxWidgets repo

   bool existed = wxFileExists(file2);
   bool result = wxCopyFile(file1, file2, overwrite) &&
      wxFile{ file1 }.Length() == wxFile{ file2 }.Length();
   if (!result && !existed)
      wxRemoveFile(file2);
   return result;

#endif
}

bool FileNames::HardLinkFile( const FilePath& file1, const FilePath& file2 )
{
#ifdef __WXMSW__

   // Fix forced ASCII conversions and wrong argument order - MJB - 29/01/2019
   //return ::CreateHardLinkA( file1.c_str(), file2.c_str(), NULL );  
   return ( 0 != ::CreateHardLink( file2, file1, NULL ) );

#else

   return 0 == ::link( file1.c_str(), file2.c_str() );

#endif
}

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
void FileNames::MakeNameUnique(FilePaths &otherNames,
   wxFileName &newName)
{
   if (otherNames.Index(newName.GetFullName(), false) >= 0) {
      int i=2;
      wxString orig = newName.GetName();
      do {
         newName.SetName(wxString::Format(wxT("%s-%d"), orig, i));
         i++;
      } while (otherNames.Index(newName.GetFullName(), false) >= 0);
   }
   otherNames.push_back(newName.GetFullName());
}



//
// Audacity user data directories
FilePath FileNames::AutoSaveDir()
{
   wxFileName autoSaveDir(FileNames::DataDir(), wxT("AutoSave"));
   return FileNames::MkDir(autoSaveDir.GetFullPath());
}

// The APP name has upercase first letter (so that Quit Audacity is correctly
// capitalised on Mac, but we want lower case APP name in paths.
// This function does that substitution, IF the last component of
// the path is 'Audacity'.
wxString FileNames::LowerCaseAppNameInPath( const wxString & dirIn){
   wxString dir = dirIn;
   // BUG 1577 Capitalisation of Audacity in path...
   if( dir.EndsWith( "Audacity" ) )
   {
      int nChars = dir.length() - wxString( "Audacity" ).length();
      dir = dir.Left( nChars ) + "audacity";
   }
   return dir;
}

FilePath FileNames::DataDir()
{
   // LLL:  Wouldn't you know that as of WX 2.6.2, there is a conflict
   //       between wxStandardPaths and wxConfig under Linux.  The latter
   //       creates a normal file as "$HOME/.audacity", while the former
   //       expects the ".audacity" portion to be a directory.
   if (gDataDir.empty())
   {
      // If there is a directory "Portable Settings" relative to the
      // executable's EXE file, the prefs are stored in there, otherwise
      // the prefs are stored in the user data dir provided by the OS.
      wxFileName exePath(PlatformCompatibility::GetExecutablePath());
#if defined(__WXMAC__)
      // Path ends for example in "Audacity.app/Contents/MacOSX"
      //exePath.RemoveLastDir();
      //exePath.RemoveLastDir();
      // just remove the MacOSX part.
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
         wxString dataDir( LowerCaseAppNameInPath( wxStandardPaths::Get().GetUserDataDir() ));
#if defined( __WXGTK__ )
         dataDir = dataDir + wxT("-data");
#endif
         gDataDir = FileNames::MkDir(dataDir);
      }
   }
   return gDataDir;
}

FilePath FileNames::ResourcesDir(){
   wxString resourcesDir( LowerCaseAppNameInPath( wxStandardPaths::Get().GetResourcesDir() ));
   return resourcesDir;
}

FilePath FileNames::HtmlHelpDir()
{
#if defined(__WXMAC__)
   wxFileName exePath(PlatformCompatibility::GetExecutablePath());
      // Path ends for example in "Audacity.app/Contents/MacOSX"
      //exePath.RemoveLastDir();
      //exePath.RemoveLastDir();
      // just remove the MacOSX part.
      exePath.RemoveLastDir();

   //for mac this puts us within the .app: Audacity.app/Contents/SharedSupport/
   return wxFileName( exePath.GetPath()+wxT("/help/manual"), wxEmptyString ).GetFullPath();
#else
   //linux goes into /*prefix*/share/audacity/
   //windows (probably) goes into the dir containing the .exe
   wxString dataDir = FileNames::LowerCaseAppNameInPath( wxStandardPaths::Get().GetDataDir());
   return wxFileName( dataDir+wxT("/help/manual"), wxEmptyString ).GetFullPath();
#endif
}

FilePath FileNames::LegacyChainDir()
{
   // Don't force creation of it
   return wxFileName{ DataDir(), wxT("Chains") }.GetFullPath();
}

FilePath FileNames::MacroDir()
{
   return FileNames::MkDir( wxFileName( DataDir(), wxT("Macros") ).GetFullPath() );
}

FilePath FileNames::NRPDir()
{
   return FileNames::MkDir( wxFileName( DataDir(), wxT("NRP") ).GetFullPath() );
}

FilePath FileNames::NRPFile()
{
   return wxFileName( NRPDir(), wxT("noisegate.nrp") ).GetFullPath();
}

FilePath FileNames::PlugInDir()
{
   return FileNames::MkDir( wxFileName( DataDir(), wxT("Plug-Ins") ).GetFullPath() );
}

FilePath FileNames::PluginRegistry()
{
   return wxFileName( DataDir(), wxT("pluginregistry.cfg") ).GetFullPath();
}

FilePath FileNames::PluginSettings()
{
   return wxFileName( DataDir(), wxT("pluginsettings.cfg") ).GetFullPath();
}

FilePath FileNames::BaseDir()
{
   wxFileName baseDir;

#if defined(__WXMAC__)
   baseDir = PlatformCompatibility::GetExecutablePath();

   // Path ends for example in "Audacity.app/Contents/MacOSX"
   //baseDir.RemoveLastDir();
   //baseDir.RemoveLastDir();
   // just remove the MacOSX part.
   baseDir.RemoveLastDir();
#elif defined(__WXMSW__)
   // Don't use wxStandardPaths::Get().GetDataDir() since it removes
   // the "Debug" directory in debug builds.
   baseDir = PlatformCompatibility::GetExecutablePath();
#else
   // Linux goes into /*prefix*/share/audacity/
   baseDir = FileNames::LowerCaseAppNameInPath(wxStandardPaths::Get().GetDataDir());
#endif

   return baseDir.GetPath();
}

FilePath FileNames::ModulesDir()
{
   wxFileName modulesDir(BaseDir(), wxEmptyString);

   modulesDir.AppendDir(wxT("modules"));

   return modulesDir.GetFullPath();
}

FilePath FileNames::ThemeDir()
{
   return FileNames::MkDir( wxFileName( DataDir(), wxT("Theme") ).GetFullPath() );
}

FilePath FileNames::ThemeComponentsDir()
{
   return FileNames::MkDir( wxFileName( ThemeDir(), wxT("Components") ).GetFullPath() );
}

FilePath FileNames::ThemeCachePng()
{
   return wxFileName( ThemeDir(), wxT("ImageCache.png") ).GetFullPath();
}

FilePath FileNames::ThemeCacheHtm()
{
   return wxFileName( ThemeDir(), wxT("ImageCache.htm") ).GetFullPath();
}

FilePath FileNames::ThemeImageDefsAsCee()
{
   return wxFileName( ThemeDir(), wxT("ThemeImageDefsAsCee.h") ).GetFullPath();
}

FilePath FileNames::ThemeCacheAsCee( )
{
// DA: Theme sourcery file name.
#ifndef EXPERIMENTAL_DA
   return wxFileName( ThemeDir(), wxT("ThemeAsCeeCode.h") ).GetFullPath();
#else
   return wxFileName( ThemeDir(), wxT("DarkThemeAsCeeCode.h") ).GetFullPath();
#endif
}

FilePath FileNames::ThemeComponent(const wxString &Str)
{
   return wxFileName( ThemeComponentsDir(), Str, wxT("png") ).GetFullPath();
}

//
// Returns the full path of program module (.exe, .dll, .so, .dylib) containing address
//
FilePath FileNames::PathFromAddr(void *addr)
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

wxFileNameWrapper FileNames::DefaultToDocumentsFolder
(const wxString &preference)
{
   wxFileNameWrapper result;
   result.AssignHomeDir();

#ifdef __WIN32__
   result.SetPath(gPrefs->Read(
      preference, result.GetPath(wxPATH_GET_VOLUME) + "\\Documents\\Audacity"));
   // The path might not exist.
   // There is no error if the path could not be created.  That's OK.
   // The dialog that Audacity offers will allow the user to select a valid directory.
   result.Mkdir(0755, wxPATH_MKDIR_FULL);
#else
   result.SetPath(gPrefs->Read( preference, result.GetPath() + "/Documents"));
#endif

   return result;
}

namespace {
   wxString PreferenceKey(FileNames::Operation op)
   {
      wxString key;
      switch (op) {
         case FileNames::Operation::Open:
            key = wxT("/DefaultOpenPath"); break;
         case FileNames::Operation::Export:
            key = wxT("/DefaultExportPath"); break;
         case FileNames::Operation::_None:
         default:
            break;
      }
      return key;
   }
}

wxString FileNames::FindDefaultPath(Operation op)
{
   auto key = PreferenceKey(op);
   if (key.empty())
      return wxString{};
   else
      return DefaultToDocumentsFolder(key).GetPath();
}

void FileNames::UpdateDefaultPath(Operation op, const FilePath &path)
{
   if (path.empty())
      return;
   auto key = PreferenceKey(op);
   if (!key.empty())  {
      gPrefs->Write(key, ::wxPathOnly(path));
      gPrefs->Flush();
   }
}

wxString
FileNames::SelectFile(Operation op,
           const wxString& message,
           const FilePath& default_path,
           const FilePath& default_filename,
           const wxString& default_extension,
           const wxString& wildcard,
           int flags,
           wxWindow *parent)
{
   return WithDefaultPath(op, default_path, [&](const FilePath &path) {
      return FileSelector(
            message, path, default_filename, default_extension,
            wildcard, flags, parent, wxDefaultCoord, wxDefaultCoord);
   });
}
