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
#include <wx/statbmp.h>
#include <wx/stattext.h>
#include <wx/utils.h>
#include <wx/html/htmlwin.h>
#include <wx/settings.h>
#include <wx/statusbr.h>
#include <wx/artprov.h>

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
   const TranslatableString & dlogTitle,
   const TranslatableString & message,
   const wxString & helpPage,
   const bool Close, const bool modal):
   wxDialogWrapper(parent, (wxWindowID)-1, dlogTitle)
{
   SetName();

   long buttonMask;

   // only add the help button if we have a URL
   buttonMask = (helpPage.empty()) ? eOkButton : (eHelpButton | eOkButton);
   dhelpPage = helpPage;
   dClose = Close;
   dModal = modal;

   ShuttleGui S(this, eIsCreating);

   S.StartHorizontalLay();
   {
      // wxART_ERROR and wxART_INFORMATION are other possibilities.
//      S.AddIcon( &wxArtProvider::GetBitmap( wxART_WARNING));
      S.SetBorder( 20 );
      wxBitmap bitmap = wxArtProvider::GetBitmap(wxART_WARNING);
      auto icon = safenew wxStaticBitmap(S.GetParent(), -1, bitmap);
      S.AddWindow( icon );
      S.StartVerticalLay();
      {
         S.SetBorder(20);
         S.AddFixedText(message, false, 500);
         S.SetBorder(2);
         S.AddStandardButtons(buttonMask);
      }
      S.EndVerticalLay();
   }
   S.EndHorizontalLay();

   Layout();
   GetSizer()->Fit(this);
   SetMinSize(GetSize());
   Center();
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
                     const TranslatableString &dlogTitle,
                     const TranslatableString &message,
                     const wxString &helpPage,
                     const bool Close)
{
   ErrorDialog dlog(parent, dlogTitle, message, helpPage, Close);
   dlog.CentreOnParent();
   dlog.ShowModal();
}


// unused.
void ShowModelessErrorDialog(wxWindow *parent,
                             const TranslatableString &dlogTitle,
                             const TranslatableString &message,
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
