/**********************************************************************

  Audacity: A Digital Audio Editor

  ErrorDialog.h

  Jimmy Johnson
  James Crook

**********************************************************************/

#ifndef __AUDACITY_ERRORDIALOG__
#define __AUDACITY_ERRORDIALOG__

#include "../Audacity.h"
#include <wx/defs.h>
#include <wx/window.h>

class AudacityProject;

/// Displays an error dialog with a button that offers help
void ShowErrorDialog(wxWindow *parent,
                     const wxString &dlogTitle,
                     const wxString &message,
                     const wxString &helpURL,
                     bool Close = true);

/// Displays a modeless error dialog with a button that offers help
void ShowModelessErrorDialog(wxWindow *parent,
                     const wxString &dlogTitle,
                     const wxString &message,
                     const wxString &helpURL,
                     bool Close = true);

/// Displays a custom modeless error dialog for aliased file errors
void ShowAliasMissingDialog(AudacityProject *parent,
                     const wxString &dlogTitle,
                     const wxString &message,
                     const wxString &helpURL,
                     const bool Close = true);

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



#endif // __AUDACITY_ERRORDIALOG__
