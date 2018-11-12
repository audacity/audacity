/**********************************************************************

  Audacity: A Digital Audio Editor

  FileNames.h

  James Crook

**********************************************************************/

#ifndef __AUDACITY_FILE_NAMES__
#define __AUDACITY_FILE_NAMES__

#include "Audacity.h"

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

   // originally an ExportMultiple method. Append suffix if newName appears in otherNames.
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

private:
   // Private constructors: No one is ever going to instantiate it.
   //
   FileNames(){;};
   ~FileNames(){;};
};

#endif
