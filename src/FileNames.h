/**********************************************************************

  Audacity: A Digital Audio Editor

  FileNames.h

  James Crook

**********************************************************************/

#ifndef __AUDACITY_FILE_NAMES__
#define __AUDACITY_FILE_NAMES__

#include <wx/string.h>
#include "Audacity.h"

class wxFileName;
class wxFileNameWrapper;
class wxArrayString;

// Uh, this is really a namespace rather than a class,
// since all the functions are static.
class AUDACITY_DLL_API FileNames
{
public:
   // This exists to compensate for bugs in wxCopyFile:
   static bool CopyFile(
      const wxString& file1, const wxString& file2, bool overwrite = true);

   // wxWidgets doesn't have a function to do this:  make a hard file-system
   // link if possible.  It might not be, as when the paths are on different
   // storage devices.
   static bool HardLinkFile( const wxString& file1, const wxString& file2);

   static wxString MkDir(const wxString &Str);
   static wxString TempDir();

   // originally an ExportMultiple method. Append suffix if newName appears in otherNames.
   static void MakeNameUnique(wxArrayString &otherNames, wxFileName &newName);

   static wxString LowerCaseAppNameInPath( const wxString & dirIn);
   /** \brief Audacity user data directory
    *
    * Where audacity keeps it's settings and other user data squirreled away,
    * by default ~/.audacity-data/ on Unix, Application Data/Audacity on
    * windows system */
   static wxString DataDir();
   static wxString ResourcesDir();
   static wxString AutoSaveDir();
   static wxString HtmlHelpDir();
   static wxString HtmlHelpIndexFile(bool quick);
   static wxString LegacyChainDir();
   static wxString MacroDir();
   static wxString NRPDir();
   static wxString NRPFile();
   static wxString PluginRegistry();
   static wxString PluginSettings();

   static wxString BaseDir();
   static wxString ModulesDir();

   /** \brief The user plug-in directory (not a system one)
    *
    * This returns the string path to where the user may have put plug-ins
    * if they don't have system admin rights. Under default settings, it's
    * <DataDir>/Plug-Ins/ */
   static wxString PlugInDir();
   static wxString ThemeDir();
   static wxString ThemeComponentsDir();
   static wxString ThemeCachePng();
   static wxString ThemeCacheAsCee();
   static wxString ThemeComponent(const wxString &Str);
   static wxString ThemeCacheHtm();
   static wxString ThemeImageDefsAsCee();

   // Obtain name of loaded module that contains address
   static wxString PathFromAddr(void *addr);

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
   static void UpdateDefaultPath(Operation op, const wxString &path);

   // F is a function taking a wxString, returning wxString
   template<typename F>
   static wxString WithDefaultPath
   (Operation op, const wxString &defaultPath, F function)
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
                const wxString& default_path,
                const wxString& default_filename,
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
