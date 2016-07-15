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
#include "wxPanelWrapper.h"

class AudacityProject;

class ErrorDialog /* not final */ : public wxDialogWrapper
{
public:
   // constructors and destructors
   ErrorDialog(wxWindow *parent,
      const wxString & dlogTitle,
      const wxString & message,
      const wxString & helpURL,
      const bool Close = true, const bool modal = true);

   virtual ~ErrorDialog(){}

private:
   wxString dhelpURL;
   bool dClose;
   bool dModal;

   void OnOk( wxCommandEvent &event );
   void OnHelp( wxCommandEvent &event );
   DECLARE_EVENT_TABLE()
};

// Helper class to make browser "simulate" a modal dialog
class HtmlTextHelpDialog final : public BrowserDialog
{
public:
   HtmlTextHelpDialog(wxWindow *pParent, const wxString &title)
      : BrowserDialog{ pParent, title }
   {
#if !wxCHECK_VERSION(3, 0, 0)
      MakeModal( true );
#endif
   }
   virtual ~HtmlTextHelpDialog()
   {
#if !wxCHECK_VERSION(3, 0, 0)
      MakeModal( false );
#endif
      // On Windows, for some odd reason, the Audacity window will be sent to
      // the back.  So, make sure that doesn't happen.
      GetParent()->Raise();
   }
};

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

#endif // __AUDACITY_ERRORDIALOG__
