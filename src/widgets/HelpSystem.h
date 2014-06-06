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

/// Displays cutable information in a text ctrl, with an OK button.
void ShowInfoDialog( wxWindow *parent,
                     const wxString &dlogTitle,
                     const wxString &shortMsg,
                     const wxString &message, 
                     const int xSize, const int ySize);

/// Displays a new window with wxHTML help.
void ShowHtmlText( wxWindow * pParent, 
                   const wxString &Title,
                   const wxString &HtmlText,
                   bool bIsFile, bool bModal);

/// Displays a file in your browser, if it's available locally,
/// OR else links to the internet.
void ShowHelpDialog(wxWindow *parent,
                     const wxString &localFileName,
                     const wxString &remoteURL);



#endif // __AUDACITY_HELPSYSTEM__
