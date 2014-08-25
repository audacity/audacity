/**********************************************************************

  Audacity: A Digital Audio Editor

  HelpSystem.cpp

  Jimmy Johnson
  Leland Lucius
  Richard Ash

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

#include "../FileNames.h"
#include "LinkingHtmlWindow.h"
#include "../Theme.h"
#include "../AllThemeResources.h"
#include "../ShuttleGui.h"
#include "../HelpText.h"
#include "../Project.h"
#include "../Prefs.h"

#include "ErrorDialog.h"
#include "HelpSystem.h"

const wxString HelpSystem::HelpHostname = wxT("manual.audacityteam.org");
const wxString HelpSystem::HelpServerDir = wxT("/o/man/");
const wxString HelpSystem::HelpAlphaDir = wxT("/man/");
const wxString HelpSystem::ReleaseSuffix = wxT(".html");

/// Mostly we use this so that we have the code for resizability
/// in one place.  Other considerations like screen readers are also
/// handled by having the code in one place.
void HelpSystem::ShowInfoDialog( wxWindow *parent,
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

void HelpSystem::ShowHtmlText(wxWindow *pParent,
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

void HelpSystem::ShowHelpDialog(wxWindow *parent,
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

   // Obtain the local file system file name, without the anchor if present.
   wxString localfile;
   if (localFileName.Find('#', true) != wxNOT_FOUND)
      localfile = localFileName.BeforeLast('#');
   else
      localfile = localFileName;

   if( localFileName.Contains(wxT("Quick_Help")) )
      // 'Quick_Help' is installed locally
      OpenInDefaultBrowser( localFileName );
   else if( (HelpMode == wxT("FromInternet")) && !remoteURL.IsEmpty() )
   {
      // Always go to remote URL.  Use External browser.
      OpenInDefaultBrowser( remoteURL );
   }
   else if( !wxFileExists( localfile ))
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

void HelpSystem::ShowHelpDialog(wxWindow *parent, const wxString &PageName)
{
   wxString localHelpPage;
   wxString webHelpPage;
   wxString releasePageName;
   wxString anchor;	// optional part of URL after (and including) the '#'
   if (PageName.Find('#', true) != wxNOT_FOUND)
   {	// need to split anchor off into separate variable
      releasePageName= PageName.BeforeLast('#');
      anchor = wxT("#") + PageName.AfterLast('#');
   }
   else
   {
      releasePageName = PageName;
      anchor = wxT("");
   }
   // This bit of code replicates the name transformations performed by the
   // clean_filename routine in scripts/mw2html_audacity/mw2html.py
   // there is a special case for transforming the front page
   // (Main_Page => index.html)
   if (releasePageName == wxT("Main_Page"))
   {
      releasePageName = wxT("index") + HelpSystem::ReleaseSuffix + anchor;
   }
   else
   {	// for any other name
   releasePageName.Replace(wxT("%%"), wxT("_"), true);
   releasePageName.Replace(wxT("%25"), wxT("_"), true);
   releasePageName.Replace(wxT("%"), wxT("_"), true);
   releasePageName.Replace(wxT("-"), wxT("_"), true);
   releasePageName.Replace(wxT("__"), wxT("_"), true);
   releasePageName.Replace(wxT("_."), wxT("."), true);
   releasePageName = releasePageName.Lower()+HelpSystem::ReleaseSuffix + anchor;
   }

   localHelpPage = wxFileName(FileNames::HtmlHelpDir()+wxT("/man/"), releasePageName).GetFullPath();
#if IS_ALPHA
   webHelpPage = wxT("http://")+HelpSystem::HelpHostname+HelpSystem::HelpAlphaDir+PageName;
#else
   webHelpPage = wxT("http://")+HelpSystem::HelpHostname+HelpSystem::HelpServerDir+releasePageName;
#endif
   wxLogMessage(wxT("Help button pressed: PageName %s, releasePageName %s"),
              PageName.c_str(), releasePageName.c_str());
   wxLogMessage(wxT("webHelpPage %s, localHelpPage %s"),
              webHelpPage.c_str(), localHelpPage.c_str());
   HelpSystem::ShowHelpDialog(
      parent, 
      localHelpPage,
      webHelpPage);
}
