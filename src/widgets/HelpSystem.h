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
   /// OR else links to the internet.
   static void ShowHelpDialog(wxWindow *parent,
                     const wxString &localFileName,
                     const wxString &remoteURL);
};

#endif // __AUDACITY_HELPSYSTEM__
