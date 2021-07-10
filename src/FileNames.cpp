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


#include "FileNames.h"



#include <memory>

#include <wx/app.h>
#include <wx/defs.h>
#include <wx/filename.h>
#include <wx/intl.h>
#include <wx/stdpaths.h>
#include "Prefs.h"
#include "Internat.h"
#include "PlatformCompatibility.h"
#include "wxFileNameWrapper.h"
#include "widgets/AudacityMessageBox.h"
#include "widgets/FileDialog/FileDialog.h"

#if defined(__WXMAC__) || defined(__WXGTK__)
#include <dlfcn.h>
#endif

#if defined(__WXMSW__)
#include <windows.h>
#endif

static wxString gDataDir;

const FileNames::FileType
     FileNames::AllFiles{ XO("All files"), { wxT("") } }
     /* i18n-hint an Audacity project is the state of the program, stored as
     files that can be reopened to resume the session later */
   , FileNames::AudacityProjects{ XO("AUP3 project files"), { wxT("aup3") }, true }
   , FileNames::DynamicLibraries{
#if defined(__WXMSW__)
      XO("Dynamically Linked Libraries"), { wxT("dll") }, true
#elif defined(__WXMAC__)
      XO("Dynamic Libraries"), { wxT("dylib") }, true
#else
      XO("Dynamically Linked Libraries"), { wxT("so*") }, true
#endif
     }
   , FileNames::TextFiles{ XO("Text files"), { wxT("txt") }, true }
   , FileNames::XMLFiles{ XO("XML files"), { wxT("xml"), wxT("XML") }, true }
;

wxString FileNames::FormatWildcard( const FileTypes &fileTypes )
{
   // |-separated list of:
   // [ Description,
   //   ( if appendExtensions, then ' (', globs, ')' ),
   //   '|',
   //   globs ]
   // where globs is a ;-separated list of filename patterns, which are
   // '*' for an empty extension, else '*.' then the extension
   // Only the part before | is displayed in the choice drop-down of file
   // dialogs
   //
   // Exceptional case: if there is only one type and its description is empty,
   // then just give the globs with no |
   // Another exception: an empty description, when there is more than one
   // type, is replaced with a default
   // Another exception:  if an extension contains a dot, it is interpreted as
   // not really an extension, but a literal filename

   const wxString dot{ '.' };
   const auto makeGlobs = [&dot]( const FileExtensions &extensions ){
      wxString globs;
      for ( const auto &extension: extensions ) {
         if ( !globs.empty() )
            globs += ';';
         if ( extension.Contains( dot ) )
            globs += extension;
         else {
            globs += '*';
            if ( !extension.empty() ) {
               globs += '.';
               globs += extension;
            }
         }
      }
      return globs;
   };

   const auto defaultDescription = []( const FileExtensions &extensions ){
      // Assume extensions is not empty
      wxString exts = extensions[0];
      for (size_t ii = 1, size = extensions.size(); ii < size; ++ii ) {
         exts += XO(", ").Translation();
         exts += extensions[ii];
      }
      /* i18n-hint a type or types such as "txt" or "txt, xml" will be
         substituted for %s */
      return XO("%s files").Format( exts );
   };

   if ( fileTypes.size() == 1 && fileTypes[0].description.empty() ) {
      return makeGlobs( fileTypes[0].extensions );
   }
   else {
      wxString result;
      for ( const auto &fileType : fileTypes ) {
         const auto &extensions = fileType.extensions;
         if (extensions.empty())
            continue;

         if (!result.empty())
            result += '|';

         const auto globs = makeGlobs( extensions );

         auto mask = fileType.description;
         if ( mask.empty() )
           mask = defaultDescription( extensions );
         if ( fileType.appendExtensions )
            mask.Join( XO("(%s)").Format( globs ), " " );
         result += mask.Translation();
         result += '|';
         result += globs;
      }
      return result;
   }
}

bool FileNames::DoCopyFile(
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

// originally an ExportMultipleDialog method. Append suffix if newName appears in otherNames.
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
   baseDir = FileNames::LowerCaseAppNameInPath(wxStandardPaths::Get().GetPluginsDir());
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


bool FileNames::IsPathAvailable( const FilePath & Path){
   if( Path.IsEmpty() )
      return false;
#ifndef __WIN32__
   return true;
#else
   wxFileNameWrapper filePath( Path );
   return filePath.DirExists() && !filePath.FileExists();
#endif
}

wxFileNameWrapper FileNames::DefaultToDocumentsFolder(const wxString &preference)
{
   wxFileNameWrapper result;

#ifdef __WIN32__
   wxFileName defaultPath( wxStandardPaths::Get().GetDocumentsDir(), "" );
   defaultPath.AppendDir( wxTheApp->GetAppName() );
   result.SetPath( gPrefs->Read( preference, defaultPath.GetPath( wxPATH_GET_VOLUME ) ) );

   // MJB: Bug 1899 & Bug 2007.  Only create directory if the result is the default path
   bool bIsDefaultPath = result == defaultPath;
   if( !bIsDefaultPath ) 
   {
      // IF the prefs directory doesn't exist - (Deleted by our user perhaps?)
      //    or exists as a file
      // THEN fallback to using the default directory.
      bIsDefaultPath = !IsPathAvailable( result.GetPath(wxPATH_GET_VOLUME|wxPATH_GET_SEPARATOR ) );
      if( bIsDefaultPath )
      {
         result.SetPath( defaultPath.GetPath( wxPATH_GET_VOLUME ) );
         // Don't write to gPrefs.
         // We typically do it later, (if directory actually gets used)
      }
   }
   if ( bIsDefaultPath )
   {
      // The default path might not exist since it is a sub-directory of 'Documents'
      // There is no error if the path could not be created.  That's OK.
      // The dialog that Audacity offers will allow the user to select a valid directory.
      result.Mkdir(0755, wxPATH_MKDIR_FULL);
   }
#else
   result.AssignHomeDir();
   result.SetPath(gPrefs->Read( preference, result.GetPath() + "/Documents"));
#endif

   return result;
}

wxString FileNames::PreferenceKey(FileNames::Operation op, FileNames::PathType type)
{
   wxString key;
   switch (op) {
      case FileNames::Operation::Temp:
         key = wxT("/Directories/TempDir"); break;
      case FileNames::Operation::Presets:
         key = wxT("/Presets/Path"); break;
      case FileNames::Operation::Open:
         key = wxT("/Directories/Open"); break;
      case FileNames::Operation::Save:
         key = wxT("/Directories/Save"); break;
      case FileNames::Operation::Import:
         key = wxT("/Directories/Import"); break;
      case FileNames::Operation::Export:
         key = wxT("/Directories/Export"); break;
      case FileNames::Operation::MacrosOut:
         key = wxT("/Directories/MacrosOut"); break;
      case FileNames::Operation::_None:
      default:
         break;
   }

   switch (type) {
      case FileNames::PathType::User:
         key += "/Default"; break;
      case FileNames::PathType::LastUsed:
         key += "/LastUsed"; break;
      case FileNames::PathType::_None:
      default:
         break;
   }

   return key;
}

FilePath FileNames::FindDefaultPath(Operation op)
{
   auto key = PreferenceKey(op, PathType::User);

   if (key.empty())
      return wxString{};

   // If the user specified a default path, then use that
   FilePath path = gPrefs->Read(key, wxT(""));
   if (!path.empty()) {
      return path;
   }

   // Maybe the last used path is available
   key = PreferenceKey(op, PathType::LastUsed);
   path = gPrefs->Read(key, wxT(""));
   if (!path.empty()) {
      return path;
   }

   // Last resort is to simply return the default folder
   return DefaultToDocumentsFolder("").GetPath();
}

void FileNames::UpdateDefaultPath(Operation op, const FilePath &path)
{
   if (path.empty())
      return;
   wxString key;
   if (op == Operation::Temp) {
      key = PreferenceKey(op, PathType::_None);
   }
   else {
      key = PreferenceKey(op, PathType::LastUsed);
   }
   if (!key.empty()) {
      gPrefs->Write(key, path);
      gPrefs->Flush();
   }
}

FilePath
FileNames::SelectFile(Operation op,
   const TranslatableString& message,
   const FilePath& default_path,
   const FilePath& default_filename,
   const FileExtension& default_extension,
   const FileTypes& fileTypes,
   int flags,
   wxWindow *parent)
{
   return WithDefaultPath(op, default_path, [&](const FilePath &path) {
      wxString filter;
      if ( !default_extension.empty() )
         filter = wxT("*.") + default_extension;
      return FileSelector(
            message.Translation(), path, default_filename, filter,
            FormatWildcard( fileTypes ),
            flags, parent, wxDefaultCoord, wxDefaultCoord);
   });
}

bool FileNames::IsMidi(const FilePath &fName)
{
   const auto extension = fName.AfterLast(wxT('.'));
   return
      extension.IsSameAs(wxT("gro"), false) ||
      extension.IsSameAs(wxT("midi"), false) ||
      extension.IsSameAs(wxT("mid"), false);
}

static FilePaths sAudacityPathList;

const FilePaths &FileNames::AudacityPathList()
{
   return sAudacityPathList;
}

void FileNames::SetAudacityPathList( FilePaths list )
{
   sAudacityPathList = std::move( list );
}

// static
void FileNames::AddUniquePathToPathList(const FilePath &pathArg,
                                          FilePaths &pathList)
{
   wxFileNameWrapper pathNorm { pathArg };
   pathNorm.Normalize();
   const wxString newpath{ pathNorm.GetFullPath() };

   for(const auto &path : pathList) {
      if (pathNorm == wxFileNameWrapper{ path })
         return;
   }

   pathList.push_back(newpath);
}

// static
void FileNames::AddMultiPathsToPathList(const wxString &multiPathStringArg,
                                          FilePaths &pathList)
{
   wxString multiPathString(multiPathStringArg);
   while (!multiPathString.empty()) {
      wxString onePath = multiPathString.BeforeFirst(wxPATH_SEP[0]);
      multiPathString = multiPathString.AfterFirst(wxPATH_SEP[0]);
      AddUniquePathToPathList(onePath, pathList);
   }
}

#include <wx/log.h>

// static
void FileNames::FindFilesInPathList(const wxString & pattern,
                                      const FilePaths & pathList,
                                      FilePaths & results,
                                      int flags)
{
   wxLogNull nolog;

   if (pattern.empty()) {
      return;
   }

   wxFileNameWrapper ff;

   for(size_t i = 0; i < pathList.size(); i++) {
      ff = pathList[i] + wxFILE_SEP_PATH + pattern;
      wxDir::GetAllFiles(ff.GetPath(), &results, ff.GetFullName(), flags);
   }
}

#if defined(__WXMSW__)
static wxCharBuffer mFilename;

//
// On Windows, wxString::mb_str() can return a NULL pointer if the
// conversion to multi-byte fails.  So, based on direction intent,
// returns a pointer to an empty string or prompts for a NEW name.
//
char *FileNames::VerifyFilename(const wxString &s, bool input)
{
   static wxCharBuffer buf;
   wxString name = s;

   if (input) {
      if ((char *) (const char *)name.mb_str() == NULL) {
         name = wxEmptyString;
      }
   }
   else {
      wxFileName ff(name);
      FileExtension ext;
      while ((char *) (const char *)name.mb_str() == NULL) {
         AudacityMessageBox(
            XO(
"The specified filename could not be converted due to Unicode character use."));

         ext = ff.GetExt();
         name = FileNames::SelectFile(FileNames::Operation::_None,
            XO("Specify New Filename:"),
            wxEmptyString,
            name,
            ext,
            { ext.empty()
               ? FileNames::AllFiles
               : FileType{ {}, { ext } }
            },
            wxFD_SAVE | wxRESIZE_BORDER,
            wxGetTopLevelParent(NULL));
      }
   }

   mFilename = name.mb_str();

   return (char *) (const char *) mFilename;
}
#endif

bool FileNames::WritableLocationCheck(const FilePath& path)
{
    bool status = wxFileName::IsDirWritable(path);

    if (!status)
    {
        AudacityMessageBox(
            XO("Directory %s does not have write permissions")
            .Format(path),
            XO("Error"),
            wxOK | wxICON_ERROR);
    }

    return status;
}

// Using this with wxStringArray::Sort will give you a list that
// is alphabetical, without depending on case.  If you use the
// default sort, you will get strings with 'R' before 'a', because it is in caps.
int FileNames::CompareNoCase(const wxString& first, const wxString& second)
{
   return first.CmpNoCase(second);
}

// Create a unique filename using the passed prefix and suffix
wxString FileNames::CreateUniqueName(const wxString &prefix,
                                     const wxString &suffix /* = wxEmptyString */)
{
   static int count = 0;

   return wxString::Format(wxT("%s %s N-%i.%s"),
                           prefix,
                           wxDateTime::Now().Format(wxT("%Y-%m-%d %H-%M-%S")),
                           ++count,
                           suffix);
}

wxString FileNames::UnsavedProjectExtension()
{
   return wxT("aup3unsaved");
}

// How to detect whether the file system of a path is FAT
// No apparent way to do it with wxWidgets
#if defined(__DARWIN__)
#include <sys/mount.h>
bool FileNames::IsOnFATFileSystem(const FilePath &path)
{
   struct statfs fs;
   if (statfs(wxPathOnly(path).c_str(), &fs))
      // Error from statfs
      return false;
   return 0 == strcmp(fs.f_fstypename, "msdos");
}
#elif defined(__linux__)
#include <sys/statfs.h>
#include "/usr/include/linux/magic.h"
bool FileNames::IsOnFATFileSystem(const FilePath &path)
{
   struct statfs fs;
   if (statfs(wxPathOnly(path).c_str(), &fs))
      // Error from statfs
      return false;
   return fs.f_type == MSDOS_SUPER_MAGIC;
}
#elif defined(_WIN32)
#include <fileapi.h>
bool FileNames::IsOnFATFileSystem(const FilePath &path)
{
   wxFileNameWrapper fileName{path};
   if (!fileName.HasVolume())
      return false;
   auto volume = AbbreviatePath(fileName) + wxT("\\");
   DWORD volumeFlags;
   wxChar volumeType[64];
   if (!::GetVolumeInformationW(
      volume.wc_str(), NULL, 0, NULL, NULL,
      &volumeFlags,
      volumeType,
      WXSIZEOF(volumeType)))
      return false;
   wxString type(volumeType);
   if (type == wxT("FAT") || type == wxT("FAT32"))
      return true;
   return false;
}
#else
bool FileNames::IsOnFATFileSystem(const FilePath &path)
{
   return false;
}
#endif

wxString FileNames::AbbreviatePath( const wxFileName &fileName )
{
   wxString target;
#ifdef __WXMSW__

   // Drive letter plus colon
   target = fileName.GetVolume() + wxT(":");

#else

   // Shorten the path, arbitrarily to 3 components
   auto path = fileName;
   path.SetFullName(wxString{});
   while(path.GetDirCount() > 3)
      path.RemoveLastDir();
   target = path.GetFullPath();

#endif
   return target;
}

