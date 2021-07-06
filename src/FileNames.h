/**********************************************************************

  Sneedacity: A Digital Audio Editor

  FileNames.h

  James Crook

**********************************************************************/

#ifndef __SNEEDACITY_FILE_NAMES__
#define __SNEEDACITY_FILE_NAMES__



#include <wx/dir.h> // for wxDIR_FILES
#include <wx/string.h> // function return value
#include "Identifier.h"
#include "Prefs.h"
#include <memory>

// Please try to support unlimited path length instead of using PLATFORM_MAX_PATH!
// Define one constant for maximum path value, so we don't have to do
// platform-specific conditionals everywhere we want to check it.
#define PLATFORM_MAX_PATH 260 // Play it safe for default, with same value as Windows' MAX_PATH.

#ifdef __WXMAC__
#undef PLATFORM_MAX_PATH
#define PLATFORM_MAX_PATH PATH_MAX
#endif

#ifdef __WXGTK__
// Some systems do not restrict the path length and therefore PATH_MAX is undefined
#ifdef PATH_MAX
#undef PLATFORM_MAX_PATH
#define PLATFORM_MAX_PATH PATH_MAX
#endif
#endif

#ifdef __WXX11__
// wxX11 should also get the platform-specific definition of PLATFORM_MAX_PATH, so do not declare here.
#endif

#ifdef __WXMSW__
#undef PLATFORM_MAX_PATH
#define PLATFORM_MAX_PATH MAX_PATH
#endif

class wxFileName;
class wxFileNameWrapper;

namespace FileNames
{
   // A description of a type of file
   struct FileType {
      FileType() = default;

      FileType( TranslatableString d, FileExtensions e, bool a = false )
         : description{ std::move( d ) }
         , extensions( std::move( e ) )
         , appendExtensions{ a }
      {}

      TranslatableString description;
      FileExtensions extensions;
      // Whether to extend the displayed description with mention of the
      // extensions:
      bool appendExtensions = false;
   };

   // Frequently used types
   extern SNEEDACITY_DLL_API const FileType
        AllFiles // *
      , SneedacityProjects // *.aup3
      , DynamicLibraries // depends on the operating system
      , TextFiles // *.txt
      , XMLFiles; // *.xml, *.XML
   
   using FileTypes = std::vector< FileType >;

   // Convert fileTypes into a single string as expected by wxWidgets file
   // selection dialog
   SNEEDACITY_DLL_API wxString FormatWildcard( const FileTypes &fileTypes );

   // This exists to compensate for bugs in wxCopyFile:
   SNEEDACITY_DLL_API bool DoCopyFile(
      const FilePath& file1, const FilePath& file2, bool overwrite = true);

   // wxWidgets doesn't have a function to do this:  make a hard file-system
   // link if possible.  It might not be, as when the paths are on different
   // storage devices.
   SNEEDACITY_DLL_API
   bool HardLinkFile( const FilePath& file1, const FilePath& file2);

   SNEEDACITY_DLL_API wxString MkDir(const wxString &Str);

   SNEEDACITY_DLL_API bool IsMidi(const FilePath &fName);

   /** \brief A list of directories that should be searched for Sneedacity files
    * (plug-ins, help files, etc.).
    *
    * On Unix this will include the directory Sneedacity was installed into,
    * plus the current user's .sneedacity-data/Plug-Ins directory.  Additional
    * directories can be specified using the SNEEDACITY_PATH environment
    * variable.  On Windows or Mac OS, this will include the directory
    * which contains the Sneedacity program. */
   SNEEDACITY_DLL_API const FilePaths &SneedacityPathList();
   SNEEDACITY_DLL_API void SetSneedacityPathList( FilePaths list );

   // originally an ExportMultipleDialog method. Append suffix if newName appears in otherNames.
   SNEEDACITY_DLL_API void MakeNameUnique(
      FilePaths &otherNames, wxFileName &newName);

   SNEEDACITY_DLL_API wxString LowerCaseAppNameInPath( const wxString & dirIn);
   /** \brief Sneedacity user data directory
    *
    * Where sneedacity keeps its settings and other user data squirreled away,
    * by default ~/.sneedacity-data/ on Unix, Application Data/Sneedacity on
    * windows system */
   SNEEDACITY_DLL_API FilePath DataDir();
   SNEEDACITY_DLL_API FilePath ResourcesDir();
   SNEEDACITY_DLL_API FilePath HtmlHelpDir();
   SNEEDACITY_DLL_API FilePath HtmlHelpIndexFile(bool quick);
   SNEEDACITY_DLL_API FilePath LegacyChainDir();
   SNEEDACITY_DLL_API FilePath MacroDir();
   SNEEDACITY_DLL_API FilePath NRPDir();
   SNEEDACITY_DLL_API FilePath NRPFile();
   SNEEDACITY_DLL_API FilePath PluginRegistry();
   SNEEDACITY_DLL_API FilePath PluginSettings();

   SNEEDACITY_DLL_API FilePath BaseDir();
   SNEEDACITY_DLL_API FilePath ModulesDir();

   /** \brief The user plug-in directory (not a system one)
    *
    * This returns the string path to where the user may have put plug-ins
    * if they don't have system admin rights. Under default settings, it's
    * <DataDir>/Plug-Ins/ */
   SNEEDACITY_DLL_API FilePath PlugInDir();
   SNEEDACITY_DLL_API FilePath ThemeDir();
   SNEEDACITY_DLL_API FilePath ThemeComponentsDir();
   SNEEDACITY_DLL_API FilePath ThemeCachePng();
   SNEEDACITY_DLL_API FilePath ThemeCacheAsCee();
   SNEEDACITY_DLL_API FilePath ThemeComponent(const wxString &Str);
   SNEEDACITY_DLL_API FilePath ThemeCacheHtm();
   SNEEDACITY_DLL_API FilePath ThemeImageDefsAsCee();

   // Obtain name of loaded module that contains address
   SNEEDACITY_DLL_API FilePath PathFromAddr(void *addr);

   SNEEDACITY_DLL_API bool IsPathAvailable( const FilePath & Path);
   SNEEDACITY_DLL_API wxFileNameWrapper DefaultToDocumentsFolder
      (const wxString &preference);

   // If not None, determines a preference key (for the default path string) to
   // be read and updated
   enum class Operation {
      // _ on None to defeat some macro that is expanding this.
      _None,

      // These do not have a specific pathtype
      Temp,
      Presets,

      // These have default/lastused pathtypes
      Open,
      Save,
      Import,
      Export,
      MacrosOut
   };

   enum class PathType {
      // _ on None to defeat some macro that is expanding this.
      _None,
      User,
      LastUsed
   };

   SNEEDACITY_DLL_API wxString PreferenceKey(FileNames::Operation op, FileNames::PathType type);

   SNEEDACITY_DLL_API FilePath FindDefaultPath(Operation op);
   SNEEDACITY_DLL_API void UpdateDefaultPath(Operation op, const FilePath &path);

   // F is a function taking a wxString, returning wxString
   template<typename F>
   FilePath WithDefaultPath
   (Operation op, const FilePath &defaultPath, F function)
   {
      auto path = gPrefs->Read(PreferenceKey(op, PathType::User), defaultPath);
      if (path.empty())
         path = FileNames::FindDefaultPath(op);
      auto result = function(path);
      FileNames::UpdateDefaultPath(op, ::wxPathOnly(result));
      return result;
   }

   SNEEDACITY_DLL_API FilePath
   SelectFile(Operation op,   // op matters only when default_path is empty
      const TranslatableString& message,
      const FilePath& default_path,
      const FilePath& default_filename,
      const FileExtension& default_extension,
      const FileTypes& fileTypes,
      int flags,
      wxWindow *parent);

   // Useful functions for working with search paths
   SNEEDACITY_DLL_API void AddUniquePathToPathList(const FilePath &path,
                                       FilePaths &pathList);
   SNEEDACITY_DLL_API void AddMultiPathsToPathList(const wxString &multiPathString,
                                       FilePaths &pathList);
   SNEEDACITY_DLL_API void FindFilesInPathList(const wxString & pattern,
                                   const FilePaths & pathList,
                                   FilePaths &results,
                                   int flags = wxDIR_FILES);

   /** \brief Protect against Unicode to multi-byte conversion failures
    * on Windows */
#if defined(__WXMSW__)
   SNEEDACITY_DLL_API char *VerifyFilename(const wxString &s, bool input = true);
#endif

   // wxString compare function for sorting case, which is needed to load correctly.
   SNEEDACITY_DLL_API int CompareNoCase(const wxString& first, const wxString& second);

   // Create a unique filename using the passed prefix and suffix
   SNEEDACITY_DLL_API wxString CreateUniqueName(const wxString &prefix,
                             const wxString &suffix = wxEmptyString);

   // File extension used for unsaved/temporary project files
   SNEEDACITY_DLL_API wxString UnsavedProjectExtension();

   SNEEDACITY_DLL_API
   bool IsOnFATFileSystem(const FilePath &path);

   SNEEDACITY_DLL_API
   //! Give enough of the path to identify the device.  (On Windows, drive letter plus ':')
   wxString AbbreviatePath(const wxFileName &fileName);
};

// Use this macro to wrap all filenames and pathnames that get
// passed directly to a system call, like opening a file, creating
// a directory, checking to see that a file exists, etc...
#if defined(__WXMSW__)
// Note, on Windows we don't define an OSFILENAME() to prevent accidental use.
// See VerifyFilename() for an explanation.
#define OSINPUT(X) FileNames::VerifyFilename(X, true)
#define OSOUTPUT(X) FileNames::VerifyFilename(X, false)
#elif defined(__WXMAC__)
#define OSFILENAME(X) ((char *) (const char *)(X).fn_str())
#define OSINPUT(X) OSFILENAME(X)
#define OSOUTPUT(X) OSFILENAME(X)
#else
#define OSFILENAME(X) ((char *) (const char *)(X).mb_str())
#define OSINPUT(X) OSFILENAME(X)
#define OSOUTPUT(X) OSFILENAME(X)
#endif

#endif
