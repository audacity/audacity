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
#include <wx/msgdlg.h> // to inherit
#include "wxPanelWrapper.h" // to inherit

class AudacityProject;

class ErrorDialog /* not final */ : public wxDialogWrapper
{
public:
   // constructors and destructors
   ErrorDialog(wxWindow *parent,
      const TranslatableString & dlogTitle,
      const TranslatableString & message,
      const wxString & helpPage,
      const bool Close = true, const bool modal = true);

   virtual ~ErrorDialog(){}

private:
   wxString dhelpPage;
   bool dClose;
   bool dModal;

   void OnOk( wxCommandEvent &event );
   void OnHelp( wxCommandEvent &event );
   DECLARE_EVENT_TABLE()
};

/// Displays an error dialog with a button that offers help
void ShowErrorDialog(wxWindow *parent,
                     const TranslatableString &dlogTitle,
                     const TranslatableString &message,
                     const wxString &helpPage,
                     bool Close = true);

/// Displays a modeless error dialog with a button that offers help
void ShowModelessErrorDialog(wxWindow *parent,
                     const TranslatableString &dlogTitle,
                     const TranslatableString &message,
                     const wxString &helpPage,
                     bool Close = true);

#include <wx/textdlg.h> // to inherit

/**************************************************************************//**
\class AudacityTextEntryDialog
\brief Wrap wxTextEntryDialog so that caption IS translatable.
********************************************************************************/
class AudacityTextEntryDialog : public wxTabTraversalWrapper< wxTextEntryDialog >
{
public:
    AudacityTextEntryDialog(
         wxWindow *parent,
         const TranslatableString& message,
         const TranslatableString& caption, // don't use = wxGetTextFromUserPromptStr,
         const wxString& value = {},
         long style = wxTextEntryDialogStyle,
         const wxPoint& pos = wxDefaultPosition)
   : wxTabTraversalWrapper< wxTextEntryDialog>(
      parent,
      message.Translation(), caption.Translation(), value, style, pos )
   {}
   
   void SetInsertionPointEnd();
   bool Show(bool show = true) override;

private:
   bool mSetInsertionPointEnd{};
};

#endif // __AUDACITY_ERRORDIALOG__
