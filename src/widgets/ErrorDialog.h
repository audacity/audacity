/**********************************************************************

  Audacity: A Digital Audio Editor

  ErrorDialog.h

  Jimmy Johnson
  James Crook

**********************************************************************/

#ifndef __AUDACITY_ERRORDIALOG__
#define __AUDACITY_ERRORDIALOG__



#include <string>
#include <wx/defs.h>
#include <wx/msgdlg.h> // to inherit
#include "wxPanelWrapper.h" // to inherit

class AudacityProject;
class wxCollapsiblePaneEvent;

class ErrorDialog /* not final */ : public wxDialogWrapper
{
public:
   // constructors and destructors
   ErrorDialog(wxWindow *parent,
      const TranslatableString & dlogTitle,
      const TranslatableString & message,
      const ManualPageID & helpPage,
      const std::wstring & log,
      const bool Close = true, const bool modal = true);

   virtual ~ErrorDialog(){}

private:
   ManualPageID dhelpPage;
   bool dClose;
   bool dModal;

   void OnPane( wxCollapsiblePaneEvent &event );
   void OnOk( wxCommandEvent &event );
   void OnHelp( wxCommandEvent &event );
   DECLARE_EVENT_TABLE()
};

/// Displays an error dialog with a button that offers help
AUDACITY_DLL_API
void ShowErrorDialog(wxWindow *parent,
                     const TranslatableString &dlogTitle,
                     const TranslatableString &message,
                     const ManualPageID &helpPage,
                     bool Close = true,
                     const std::wstring &log = {});

/// Displays an error dialog, possibly allowing to send error report.
AUDACITY_DLL_API
void ShowExceptionDialog(
   wxWindow* parent, const TranslatableString& dlogTitle,
   const TranslatableString& message, const wxString& helpPage,
   bool Close = true, const wxString& log = {});

/// Displays a modeless error dialog with a button that offers help
void ShowModelessErrorDialog(wxWindow *parent,
                     const TranslatableString &dlogTitle,
                     const TranslatableString &message,
                     const ManualPageID &helpPage,
                     bool Close = true,
                     const std::wstring &log = {});

#endif // __AUDACITY_ERRORDIALOG__
