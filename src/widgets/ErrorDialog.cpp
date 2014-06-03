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

#include "ErrorDialog.h"


class ErrorDialog : public wxDialog
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

// special case for alias missing dialog because we keep track of if it exists.
class AliasedFileMissingDialog : public ErrorDialog
{
   public:
   AliasedFileMissingDialog(AudacityProject *parent,
      const wxString & dlogTitle,
      const wxString & message,
      const wxString & helpURL,
      const bool Close = true, const bool modal = true);
   virtual ~AliasedFileMissingDialog();
};

BEGIN_EVENT_TABLE(ErrorDialog, wxDialog)
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
   wxDialog(parent, (wxWindowID)-1, dlogTitle)
{
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
   Fit();
   SetMinSize(GetSize());
   Center();

#if 0
   // Original non ShuttleGui based code.
   // Layout did not look good on Windows.
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer *vSizer = new wxBoxSizer(wxVERTICAL);

   wxBoxSizer *hSizer = new wxBoxSizer(wxHORIZONTAL);

   wxStaticText *statText = new wxStaticText(this, -1, message);
   mainSizer->Add(statText, 0, wxALIGN_LEFT|wxALL, 5);

   wxButton *help = new wxButton(this, wxID_HELP, _("Help"));
   hSizer->Add(help, 0, wxALIGN_LEFT|wxALL, 5);

   wxButton *ok = new wxButton(this, wxID_OK, _("OK"));
   ok->SetDefault();
   ok->SetFocus();
   hSizer->Add(ok, 0, wxALIGN_RIGHT|wxALL, 5);

   vSizer->Add(hSizer, 0, wxALIGN_CENTER|wxALL, 5);

   mainSizer->Add(vSizer, 0, wxALL, 15 );

   SetAutoLayout(true);
   SetSizer(mainSizer);
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

// Helper class to make browser "simulate" a modal dialog
class HtmlTextHelpDialog : public BrowserFrame
{
public:
   HtmlTextHelpDialog() : BrowserFrame()
   {
      MakeModal( true );
   }
   virtual ~HtmlTextHelpDialog()
   {
      MakeModal( false );
      // On Windows, for some odd reason, the Audacity window will be sent to
      // the back.  So, make sure that doesn't happen.
      GetParent()->Raise();
   }
};

void ShowHtmlText(wxWindow *pParent,
                  const wxString &Title,
                  const wxString &HtmlText,
                  bool bIsFile = false, bool bModal = false)
{
   LinkingHtmlWindow *html;

   BrowserFrame * pWnd;
   if( bModal )
      pWnd = new HtmlTextHelpDialog();
   else
      pWnd = new BrowserFrame();

   pWnd->Create(pParent, wxID_ANY, Title, wxDefaultPosition, wxDefaultSize,
#if defined(__WXMAC__)
      // On OSX, the html frame can go behind the help dialog and if the help
      // html frame is modal, you can't get back to it.  Pressing escape gets
      // you out of this, but it's just easier to add the wxSTAY_ON_TOP flag
      // to prevent it from falling behind the dialog.  Not the perfect solution
      // but acceptable in this case.
      wxSTAY_ON_TOP |
#endif
      wxDEFAULT_FRAME_STYLE);

   ShuttleGui S( pWnd, eIsCreating );

   S.SetStyle( wxNO_BORDER | wxTAB_TRAVERSAL );
   wxPanel *pPan = S.Prop(true).StartPanel();
   {
      S.StartHorizontalLay( wxEXPAND, false );
      {
         wxButton * pWndBackwards = S.Id( wxID_BACKWARD ).AddButton( _("<") );
         wxButton * pWndForwards  = S.Id( wxID_FORWARD  ).AddButton( _(">") );
         pWndForwards->Enable( false );
         pWndBackwards->Enable( false );
         #if wxUSE_TOOLTIPS
         pWndForwards->SetToolTip( _("Forwards" ));
         pWndBackwards->SetToolTip( _("Backwards" ));
         #endif
      }
      S.EndHorizontalLay();

      html = new LinkingHtmlWindow(pPan, wxID_ANY,
                                   wxDefaultPosition,
                                   bIsFile ? wxSize(500, 400) : wxSize(480, 240),
                                   wxHW_SCROLLBAR_AUTO | wxSUNKEN_BORDER);

      html->SetRelatedFrame( pWnd, wxT("Help: %s") );
      if( bIsFile )
         html->LoadFile( HtmlText );
      else
         html->SetPage( HtmlText);

      S.Prop(1).AddWindow( html, wxEXPAND );

      S.Id( wxID_CANCEL ).AddButton( _("Close") )->SetDefault();
   }
   S.EndPanel();

   // -- START of ICON stuff -----
   // If this section (providing an icon) causes compilation errors on linux, comment it out for now.
   // it will just mean that the icon is missing.  Works OK on Windows.
   #ifdef __WXMSW__
      wxIcon ic(wxICON(AudacityLogo));
   #else
      wxIcon ic;
      ic.CopyFromBitmap(theTheme.Bitmap(bmpAudacityLogo48x48));
   #endif
   pWnd->SetIcon( ic );
   // -- END of ICON stuff -----


   pWnd->mpHtml = html;
   pWnd->SetBackgroundColour( wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE));
   pWnd->CreateStatusBar();
   pWnd->Centre();
   pWnd->Layout();
   pWnd->Fit();
   pWnd->SetSizeHints(pWnd->GetSize());
   pWnd->Show( true );

   html->SetRelatedStatusBar( 0 );
   html->SetFocus();

   return;
}

void ErrorDialog::OnHelp(wxCommandEvent & WXUNUSED(event))
{
   if( dhelpURL.StartsWith(wxT("innerlink:")) )
   {
      ShowHtmlText(
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
   ErrorDialog *dlog = new ErrorDialog(parent, dlogTitle, message, helpURL, Close, false);
   dlog->CentreOnParent();
   dlog->Show();
   // ANSWER-ME: Vigilant Sentry flags this method as not deleting dlog, so a mem leak.
   // ANSWER-ME: This is unused. Delete it or are there plans for it?
}

void ShowAliasMissingDialog(AudacityProject *parent,
                            const wxString &dlogTitle,
                            const wxString &message,
                            const wxString &helpURL,
                            const bool Close)
{
   ErrorDialog *dlog = new AliasedFileMissingDialog(parent, dlogTitle, message, helpURL, Close, false);
   // Don't center because in many cases (effect, export, etc) there will be a progress bar in the center that blocks this.
   // instead put it just above or on the top of the project.
   wxPoint point;
   point.x = 0;

   point.y = parent ? parent->GetPosition().y - 200 : 100;

   if (point.y < 100)
      point.y = 100;
   dlog->SetPosition(point);
   dlog->CentreOnParent(wxHORIZONTAL);

   dlog->Show();
   // ANSWER-ME: Vigilant Sentry flags this method as not deleting dlog, so a mem leak.
   // ANSWER-ME: Why is this modeless? Shouldn't it require user action before proceeding?
}

/// Mostly we use this so that we have the code for resizability
/// in one place.  Other considerations like screen readers are also
/// handled by having the code in one place.
void ShowInfoDialog( wxWindow *parent,
                     const wxString &dlogTitle,
                     const wxString &shortMsg,
                     const wxString &message,
                     const int xSize, const int ySize)
{
   wxDialog dlog(parent, wxID_ANY,
                dlogTitle,
                wxDefaultPosition, wxDefaultSize,
                wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER | wxMAXIMIZE_BOX /*| wxDEFAULT_FRAME_STYLE */);

   ShuttleGui S(&dlog, eIsCreating);

   S.StartVerticalLay(1);
   {
      S.AddTitle( shortMsg);
      S.SetStyle( wxTE_MULTILINE | wxTE_READONLY | wxTE_RICH | wxTE_RICH2 |
         wxTE_AUTO_URL | wxTE_NOHIDESEL | wxHSCROLL );
      S.AddTextWindow(message);
   }
   S.SetBorder( 0 );
   S.StartHorizontalLay(wxALIGN_CENTER|wxALIGN_BOTTOM, 0);
   S.AddStandardButtons(eOkButton);

   S.EndHorizontalLay();

   // Next three lines add a tiny dragger.
   wxStatusBar * pBar = new wxStatusBar( &dlog );
   pBar->SetSize( 18, 38);
   S.AddWindow( pBar, wxALIGN_BOTTOM|wxALIGN_RIGHT );

   S.EndVerticalLay();

   // Smallest size is half default size.  Seems reasonable.
   dlog.SetMinSize( wxSize(xSize/2, ySize/2) );
   dlog.SetSize( wxSize(xSize, ySize) );
   dlog.Center();
   dlog.ShowModal();
}


void ShowHelpDialog(wxWindow *parent,
                    const wxString &localFileName,
                    const wxString &remoteURL)
{
   AudacityProject * pProj = GetActiveProject();
   wxString HelpMode = wxT("Local");

   if( pProj )
   {
      HelpMode = pProj->mHelpPref;
      // these next lines are for legacy cfg files (pre 2.0) where we had different modes
      if( (HelpMode == wxT("Standard")) || (HelpMode == wxT("InBrowser")) )
      {
         HelpMode = wxT("Local");
         pProj->mHelpPref = HelpMode;
         gPrefs->Write(wxT("/GUI/Help"), HelpMode);
         gPrefs->Flush();
      }
   }

   if( localFileName.Contains(wxT("Quick_Help")) )
      // 'Quick_Help' is installed locally
      OpenInDefaultBrowser( localFileName );
   else if( (HelpMode == wxT("FromInternet")) && !remoteURL.IsEmpty() )
   {
      // Always go to remote URL.  Use External browser.
      OpenInDefaultBrowser( remoteURL );
   }
   else if( !wxFileExists( localFileName ))
   {
      // If you give an empty remote URL, you should have already ensured
      // that the file exists!
      wxASSERT( !remoteURL.IsEmpty() );
      // I can't find it'.
      // Use Built-in browser to suggest you use the remote url.
//use the remote link
      wxString Text = HelpText( wxT("remotehelp") );
      Text.Replace( wxT("*URL*"), remoteURL );
      ShowHtmlText( parent, _("Help on the Internet"), Text );
   }
   else if( HelpMode == wxT("Local") )
   {
      // Local file, External browser
      OpenInDefaultBrowser( wxString(wxT("file:"))+localFileName );
   }
   else
   {
      // Local file, Built-in browser
      ShowHtmlText( parent, wxT(""), localFileName, true );
   }
}
