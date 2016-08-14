/**********************************************************************

  Audacity: A Digital Audio Editor

  ErrorDialog.cpp

  Jimmy Johnson
  Leland Lucius

*******************************************************************//**

\class ErrorDialog
Gives an Error message with an option for help.

*//********************************************************************/

#include "../Audacity.h"

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

#include "LinkingHtmlWindow.h"
#include "../Theme.h"
#include "../AllThemeResources.h"
#include "../ShuttleGui.h"
#include "../HelpText.h"
#include "../Internat.h"
#include "../Project.h"
#include "../Prefs.h"
#include "HelpSystem.h"

#include "ErrorDialog.h"

// special case for alias missing dialog because we keep track of if it exists.
class AliasedFileMissingDialog final : public ErrorDialog
{
   public:
   AliasedFileMissingDialog(AudacityProject *parent,
      const wxString & dlogTitle,
      const wxString & message,
      const wxString & helpURL,
      const bool Close = true, const bool modal = true);
   virtual ~AliasedFileMissingDialog();
};

BEGIN_EVENT_TABLE(ErrorDialog, wxDialogWrapper)
   EVT_BUTTON( wxID_OK, ErrorDialog::OnOk)
   EVT_BUTTON( wxID_HELP, ErrorDialog::OnHelp)
END_EVENT_TABLE()


AliasedFileMissingDialog::AliasedFileMissingDialog(AudacityProject *parent,
      const wxString & dlogTitle,
      const wxString & message,
      const wxString & helpURL,
      const bool Close, const bool modal):
ErrorDialog(parent, dlogTitle, message, helpURL, Close, modal)
{
   parent->SetMissingAliasFileDialog(this);
}

AliasedFileMissingDialog::~AliasedFileMissingDialog()
{
   ((AudacityProject*)GetParent())->SetMissingAliasFileDialog(NULL);
}

ErrorDialog::ErrorDialog(
   wxWindow *parent,
   const wxString & dlogTitle,
   const wxString & message,
   const wxString & helpURL,
   const bool Close, const bool modal):
   wxDialogWrapper(parent, (wxWindowID)-1, dlogTitle)
{
   SetName(GetTitle());

   long buttonMask;

   // only add the help button if we have a URL
   buttonMask = (helpURL == wxT("")) ? eOkButton : (eHelpButton | eOkButton);
   dhelpURL = helpURL;
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
   if( dhelpURL.StartsWith(wxT("innerlink:")) )
   {
      HelpSystem::ShowHtmlText(
         this,
         TitleText(dhelpURL.Mid( 10 ) ),
         HelpText( dhelpURL.Mid( 10 )),
         false,
         true );
      return;
   }
   OpenInDefaultBrowser( dhelpURL );
   if(dClose)
      EndModal(true);
}

void ShowErrorDialog(wxWindow *parent,
                     const wxString &dlogTitle,
                     const wxString &message,
                     const wxString &helpURL,
                     const bool Close)
{
   ErrorDialog dlog(parent, dlogTitle, message, helpURL, Close);
   dlog.CentreOnParent();
   dlog.ShowModal();
}

void ShowModelessErrorDialog(wxWindow *parent,
                             const wxString &dlogTitle,
                             const wxString &message,
                             const wxString &helpURL,
                             const bool Close)
{
   wxASSERT(parent);
   ErrorDialog *dlog = safenew ErrorDialog(parent, dlogTitle, message, helpURL, Close, false);
   dlog->CentreOnParent();
   dlog->Show();
   // ANSWER-ME: Vigilant Sentry flags this method as not deleting dlog, so a mem leak.
   // ANSWER-ME: This is unused. Delete it or are there plans for it?
   // PRL: answer is that the parent window guarantees destruction of the dialog
   // but in practice Destroy() in OnOK does that
}

void ShowAliasMissingDialog(AudacityProject *parent,
                            const wxString &dlogTitle,
                            const wxString &message,
                            const wxString &helpURL,
                            const bool Close)
{
   wxASSERT(parent); // to justify safenew
   ErrorDialog *dlog = safenew AliasedFileMissingDialog(parent, dlogTitle, message, helpURL, Close, false);
   // Don't center because in many cases (effect, export, etc) there will be a progress bar in the center that blocks this.
   // instead put it just above or on the top of the project.
   wxPoint point;
   point.x = 0;

   point.y = parent ? parent->GetPosition().y - 200 : 100;

   if (point.y < 100)
      point.y = 100;
   dlog->SetPosition(point);
   dlog->CentreOnParent(wxHORIZONTAL);

   // This needs to be modeless because user may need to
   // stop playback AND read dialog's instructions.
   dlog->Show();
   // ANSWER-ME: Vigilant Sentry flags this method as not deleting dlog, so a mem leak.
   // PRL: answer is that the parent window guarantees destruction of the dialog
   // but in practice Destroy() in OnOK does that
}
