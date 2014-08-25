/**********************************************************************

  Audacity: A Digital Audio Editor

  ErrorDialog.h

  Jimmy Johnson
  James Crook

**********************************************************************/

#ifndef __AUDACITY_HELPSYSTEM__
#define __AUDACITY_HELPSYSTEM__

#include "../Audacity.h"
#include <wx/defs.h>
#include <wx/window.h>

class AudacityProject;

/** @brief Class which contains static methods and data needed for implementing
 * help buttons
 *
 * This class should be the only place in the codebase where the location of
 * the online copy of the Audacity manual is stored, so that it can be 
 * changed if required
 */
class HelpSystem
{
public:
   /// Displays cutable information in a text ctrl, with an OK button.
   static void ShowInfoDialog( wxWindow *parent,
                     const wxString &dlogTitle,
                     const wxString &shortMsg,
                     const wxString &message, 
                     const int xSize, const int ySize);

   /// Displays a new window with wxHTML help.
   static void ShowHtmlText( wxWindow * pParent, 
                   const wxString &Title,
                   const wxString &HtmlText,
                   bool bIsFile, bool bModal);

   /// Displays a file in your browser, if it's available locally,
   /// OR else links to the internet. Generally using this outside this class
   /// is depreciated in favour of the "smarter" overload below, unless there
   /// is a good reason for using this form.
   /// @param localFileName Name and path of the file on the local machine
   /// file system to be opened. file.name#anchor syntax is allowed, and therefore
   /// file names containing a '#' are not (on any platform).
   static void ShowHelpDialog(wxWindow *parent,
                     const wxString &localFileName,
                     const wxString &remoteURL);

   /// Displays a page from the Audacity manual  in your browser, if
   /// it's available locally, OR else links to the internet.
   /// @param PageName The name of the manual page to display as it is in
   /// _development version_ of the manual (i.e. in MediaWiki), _not_ the
   /// converted file name used for offline and released manuals
   static void ShowHelpDialog(wxWindow *parent,
                     const wxString &PageName);

   /// Hostname (domain name including subdomain) of the server on which the
   /// online help is available
   static const wxString HelpHostname;
   /// URL path on the help server under which the help pages are located. Must
   /// both start and end with '/' characters.
   static const wxString HelpServerDir;
   /// URL path on the help server under which the development help pages are
   /// located. Must both start and end with '/' characters.
   static const wxString HelpAlphaDir;
   /// The string which is appended to the development manual page name in order
   /// obtain the file name in the local and release web copies of the manual
   static const wxString ReleaseSuffix;

};

#endif // __AUDACITY_HELPSYSTEM__
