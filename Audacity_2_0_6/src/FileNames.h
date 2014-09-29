/**********************************************************************

  Audacity: A Digital Audio Editor

  FileNames.h

  James Crook

**********************************************************************/

#ifndef __AUDACITY_FILE_NAMES__
#define __AUDACITY_FILE_NAMES__

#include <wx/string.h>

class wxFileName;
class wxArrayString;

// Uh, this is really a namespace rather than a class,
// since all the functions are static.
class AUDACITY_DLL_API FileNames
{
public:
   static wxString MkDir(const wxString &Str);
   static wxString TempDir();

   // originally an ExportMultiple method. Append suffix if newName appears in otherNames.
   static void MakeNameUnique(wxArrayString &otherNames, wxFileName &newName);

   /** \brief Audacity user data directory
    *
    * Where audacity keeps it's settings and other user data squirreled away,
    * by default ~/.audacity-data/ on Unix, Application Data/Audacity on
    * windows system */
   static wxString DataDir();
   static wxString AutoSaveDir();
   static wxString HtmlHelpDir();
   static wxString HtmlHelpIndexFile(bool quick);
   static wxString ChainDir();
   static wxString NRPDir();
   static wxString NRPFile();
   static wxString PluginsCache();

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

private:
   // Private constructors: No one is ever going to instantiate it.
   //
   FileNames(){;};
   ~FileNames(){;};
};

#endif
