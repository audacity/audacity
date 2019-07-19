/**********************************************************************

  Audacity: A Digital Audio Editor

  ErrorDialog.cpp

  Jimmy Johnson
  Leland Lucius

*******************************************************************//**

\class ErrorDialog
\brief Gives an Error message with an option for help.

*//********************************************************************/

#include "../Audacity.h"
#include "ErrorDialog.h"

#include <wx/app.h>
#include <wx/button.h>
#include <wx/icon.h>
#include <wx/dialog.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/utils.h>
#include <wx/html/htmlwin.h>
#include <wx/settings.h>
#include <wx/statusbr.h>

#include "../AllThemeResources.h"
#include "../ShuttleGui.h"
#include "../HelpText.h"
#include "../Prefs.h"
#include "HelpSystem.h"

BEGIN_EVENT_TABLE(ErrorDialog, wxDialogWrapper)
   EVT_BUTTON( wxID_OK, ErrorDialog::OnOk)
   EVT_BUTTON( wxID_HELP, ErrorDialog::OnHelp)
END_EVENT_TABLE()

ErrorDialog::ErrorDialog(
   wxWindow *parent,
   const wxString & dlogTitle,
   const wxString & message,
   const wxString & helpPage,
   const bool Close, const bool modal):
   wxDialogWrapper(parent, (wxWindowID)-1, dlogTitle)
{
   SetName(GetTitle());

   long buttonMask;

   // only add the help button if we have a URL
   buttonMask = (helpPage.empty()) ? eOkButton : (eHelpButton | eOkButton);
   dhelpPage = helpPage;
   dClose = Close;
   dModal = modal;

   ShuttleGui S(this, eIsCreating);

   S.StartVerticalLay();
   {
      S.SetBorder( 20 );
      S.AddFixedText( message );
      S.SetBorder( 2 );
      S.AddStandardButtons( buttonMask );
   }
   S.EndVerticalLay();

   Layout();
   GetSizer()->Fit(this);
   SetMinSize(GetSize());
   Center();

#if 0
   // Original non ShuttleGui based code.
   // Layout did not look good on Windows.
   wxBoxSizer mainSizer;
   {
      auto uMainSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
      mainSizer = uMainSizer.get();
      auto vSizer = make_unique<xBoxSizer>(wxVERTICAL);

      auto hSizer = make_unique<wxBoxSizer>(wxHORIZONTAL);

      wxStaticText *statText = safenew wxStaticText(this, -1, message);
      mainSizer->Add(statText, 0, wxALIGN_LEFT|wxALL, 5);

      wxButton *help = safenew wxButton(this, wxID_HELP, _("Help"));
      hSizer->Add(help, 0, wxALIGN_LEFT|wxALL, 5);

      wxButton *ok = safenew wxButton(this, wxID_OK, _("OK"));
      ok->SetDefault();
      ok->SetFocus();
      hSizer->Add(ok, 0, wxALIGN_RIGHT|wxALL, 5);

      vSizer->Add(hSizer.release(), 0, wxALIGN_CENTER|wxALL, 5);

      mainSizer->Add(vSizer.release(), 0, wxALL, 15 );

      SetAutoLayout(true);
      SetSizer(uMainSizer.release());
   }

   mainSizer->Fit(this);
   mainSizer->SetSizeHints(this);
#endif
}

void ErrorDialog::OnOk(wxCommandEvent & WXUNUSED(event))
{
   if (dModal)
      EndModal(true);
   else
      Destroy();
}


void ErrorDialog::OnHelp(wxCommandEvent & WXUNUSED(event))
{
   if( dhelpPage.StartsWith(wxT("innerlink:")) )
   {
      HelpSystem::ShowHtmlText(
         this,
         TitleText(dhelpPage.Mid( 10 ) ),
         HelpText( dhelpPage.Mid( 10 )),
         false,
         true );
      return;
   }
   HelpSystem::ShowHelp( this, dhelpPage, dClose );
   //OpenInDefaultBrowser( dhelpURL );
   if(dClose)
      EndModal(true);
}

void ShowErrorDialog(wxWindow *parent,
                     const wxString &dlogTitle,
                     const wxString &message,
                     const wxString &helpPage,
                     const bool Close)
{
   ErrorDialog dlog(parent, dlogTitle, message, helpPage, Close);
   dlog.CentreOnParent();
   dlog.ShowModal();
}


// unused.
void ShowModelessErrorDialog(wxWindow *parent,
                             const wxString &dlogTitle,
                             const wxString &message,
                             const wxString &helpPage,
                             const bool Close)
{
   // ensure it has some parent.
   if( !parent )
      parent = wxTheApp->GetTopWindow();
   wxASSERT(parent);
   ErrorDialog *dlog = safenew ErrorDialog(parent, dlogTitle, message, helpPage, Close, false);
   dlog->CentreOnParent();
   dlog->Show();
   // ANSWER-ME: Vigilant Sentry flagged this method as not deleting dlog, so 
   // is this actually a mem leak.
   // PRL: answer is that the parent window guarantees destruction of the dialog
   // but in practice Destroy() in OnOK does that
}

void AudacityTextEntryDialog::SetInsertionPointEnd()
{
   mSetInsertionPointEnd = true;
}

bool AudacityTextEntryDialog::Show(bool show)
{
   bool ret = wxTabTraversalWrapper< wxTextEntryDialog >::Show(show);

   if (show && mSetInsertionPointEnd) {
      // m_textctrl is protected member of wxTextEntryDialog
      m_textctrl->SetInsertionPointEnd();
   }

   return ret;
}
