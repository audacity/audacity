/**********************************************************************

  Audacity: A Digital Audio Editor

  FileNames.h

  James Crook

**********************************************************************/

#ifndef __AUDACITY_FILE_NAMES__
#define __AUDACITY_FILE_NAMES__

#include "Audacity.h"

#include <wx/dir.h> // for wxDIR_FILES
#include <wx/string.h> // function return value
#include "audacity/Types.h"

class wxFileName;
class wxFileNameWrapper;

// Uh, this is really a namespace rather than a class,
// since all the functions are static.
class AUDACITY_DLL_API FileNames
{
public:
   // This exists to compensate for bugs in wxCopyFile:
   static bool CopyFile(
      const FilePath& file1, const FilePath& file2, bool overwrite = true);

   // wxWidgets doesn't have a function to do this:  make a hard file-system
   // link if possible.  It might not be, as when the paths are on different
   // storage devices.
   static bool HardLinkFile( const FilePath& file1, const FilePath& file2);

   static wxString MkDir(const wxString &Str);
   static wxString TempDir();

   static const FilePath &DefaultTempDir();
   static void SetDefaultTempDir( const FilePath &tempDir );
   static bool IsTempDirectoryNameOK( const FilePath & Name );

   static bool IsMidi(const FilePath &fName);

   /** \brief A list of directories that should be searched for Audacity files
    * (plug-ins, help files, etc.).
    *
    * On Unix this will include the directory Audacity was installed into,
    * plus the current user's .audacity-data/Plug-Ins directory.  Additional
    * directories can be specified using the AUDACITY_PATH environment
    * variable.  On Windows or Mac OS, this will include the directory
    * which contains the Audacity program. */
   static const FilePaths &AudacityPathList();
   static void SetAudacityPathList( FilePaths list );

   // originally an ExportMultipleDialog method. Append suffix if newName appears in otherNames.
   static void MakeNameUnique(
      FilePaths &otherNames, wxFileName &newName);

   static wxString LowerCaseAppNameInPath( const wxString & dirIn);
   /** \brief Audacity user data directory
    *
    * Where audacity keeps it's settings and other user data squirreled away,
    * by default ~/.audacity-data/ on Unix, Application Data/Audacity on
    * windows system */
   static FilePath DataDir();
   static FilePath ResourcesDir();
   static FilePath AutoSaveDir();
   static FilePath HtmlHelpDir();
   static FilePath HtmlHelpIndexFile(bool quick);
   static FilePath LegacyChainDir();
   static FilePath MacroDir();
   static FilePath NRPDir();
   static FilePath NRPFile();
   static FilePath PluginRegistry();
   static FilePath PluginSettings();

   static FilePath BaseDir();
   static FilePath ModulesDir();

   /** \brief The user plug-in directory (not a system one)
    *
    * This returns the string path to where the user may have put plug-ins
    * if they don't have system admin rights. Under default settings, it's
    * <DataDir>/Plug-Ins/ */
   static FilePath PlugInDir();
   static FilePath ThemeDir();
   static FilePath ThemeComponentsDir();
   static FilePath ThemeCachePng();
   static FilePath ThemeCacheAsCee();
   static FilePath ThemeComponent(const wxString &Str);
   static FilePath ThemeCacheHtm();
   static FilePath ThemeImageDefsAsCee();

   // Obtain name of loaded module that contains address
   static FilePath PathFromAddr(void *addr);

   static bool IsPathAvailable( const FilePath & Path);
   static wxFileNameWrapper DefaultToDocumentsFolder
      (const wxString &preference);

   // If not None, determines a preference key (for the default path string) to
   // be read and updated
   enum class Operation {
      // _ on None to defeat some macro that is expanding this.
      _None,
      Open,
      Export
   };

   static wxString FindDefaultPath(Operation op);
   static void UpdateDefaultPath(Operation op, const FilePath &path);

   // F is a function taking a wxString, returning wxString
   template<typename F>
   static wxString WithDefaultPath
   (Operation op, const FilePath &defaultPath, F function)
   {
      auto path = defaultPath;
      if (path.empty())
         path = FileNames::FindDefaultPath(op);
      auto result = function(path);
      FileNames::UpdateDefaultPath(op, result);
      return result;
   }

   static wxString
   SelectFile(Operation op,   // op matters only when default_path is empty
              const wxString& message,
                const FilePath& default_path,
                const FilePath& default_filename,
                // empty, or one extension, or multiple extensions joined with
                // '|', extensions including the leading dot:
                const wxString& default_extension,
                const wxString& wildcard,
                int flags,
                wxWindow *parent);

   // Useful functions for working with search paths
   static void AddUniquePathToPathList(const FilePath &path,
                                       FilePaths &pathList);
   static void AddMultiPathsToPathList(const wxString &multiPathString,
                                       FilePaths &pathList);
   static void FindFilesInPathList(const wxString & pattern,
                                   const FilePaths & pathList,
                                   FilePaths &results,
                                   int flags = wxDIR_FILES);

   /** \brief Protect against Unicode to multi-byte conversion failures
    * on Windows */
#if defined(__WXMSW__)
   static char *VerifyFilename(const wxString &s, bool input = true);
   
   // stuff for file name sanitisation
   static wxCharBuffer mFilename;
#endif

private:
   // Private constructors: No one is ever going to instantiate it.
   //
   FileNames(){;};
   ~FileNames(){;};
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
