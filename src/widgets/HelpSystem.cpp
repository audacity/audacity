/**********************************************************************

  Audacity: A Digital Audio Editor

  HelpSystem.cpp

  Jimmy Johnson
  Leland Lucius
  Richard Ash

  was merged with LinkingHtmlWindow.h

  Vaughan Johnson
  Dominic Mazzoni

  utility fn and
  descendant of HtmlWindow that opens links in the user's
  default browser

*//********************************************************************/


#include "HelpSystem.h"

#include <wx/setup.h> // for wxUSE_* macros
#include <wx/button.h>
#include <wx/frame.h>
#include <wx/icon.h>
#include <wx/dialog.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/utils.h>
#include <wx/html/htmlwin.h>
#include <wx/settings.h>
#include <wx/statusbr.h>
#include <wx/regex.h>

#include "FileNames.h"
#include "../AllThemeResources.h"
#include "../ShuttleGui.h"
#include "../Theme.h"
#include "../HelpText.h"
#include "Prefs.h"
#include "wxFileNameWrapper.h"
#include "../prefs/GUIPrefs.h"

#ifdef USE_ALPHA_MANUAL
const wxString HelpSystem::HelpHostname = wxT("alphamanual.audacityteam.org");
const wxString HelpSystem::HelpServerHomeDir = wxT("/man/");
const wxString HelpSystem::HelpServerManDir = wxT("/man/");
#else
const wxString HelpSystem::HelpHostname = wxT("manual.audacityteam.org");
const wxString HelpSystem::HelpServerHomeDir = wxT("/");
const wxString HelpSystem::HelpServerManDir = wxT("/man/");
#endif
const wxString HelpSystem::LocalHelpManDir = wxT("/man/");

namespace {

// Helper class to make browser "simulate" a modal dialog
class HtmlTextHelpDialog final : public BrowserDialog
{
public:
   HtmlTextHelpDialog(wxWindow *pParent, const TranslatableString &title)
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

}

/// Mostly we use this so that we have the code for resizability
/// in one place.  Other considerations like screen readers are also
/// handled by having the code in one place.
void HelpSystem::ShowInfoDialog( wxWindow *parent,
                     const TranslatableString &dlogTitle,
                     const TranslatableString &shortMsg,
                     const wxString &message,
                     const int xSize, const int ySize)
{
   wxDialogWrapper dlog(parent, wxID_ANY,
                dlogTitle,
                wxDefaultPosition, wxDefaultSize,
                wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER | wxMAXIMIZE_BOX /*| wxDEFAULT_FRAME_STYLE */);

   dlog.SetName();
   ShuttleGui S(&dlog, eIsCreating);

   S.StartVerticalLay(1);
   {
      S.AddTitle( shortMsg );
      S.Style( wxTE_MULTILINE | wxTE_READONLY | wxTE_RICH | wxTE_RICH2 |
              wxTE_AUTO_URL | wxTE_NOHIDESEL | wxHSCROLL )
         .AddTextWindow(message);

      S.SetBorder( 0 );
      S.StartHorizontalLay(wxALIGN_CENTER_HORIZONTAL, 0);
         S.AddStandardButtons(eOkButton);
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   // Smallest size is half default size.  Seems reasonable.
   dlog.SetMinSize( wxSize(xSize/2, ySize/2) );
   dlog.SetSize( wxSize(xSize, ySize) );
   dlog.Center();
   dlog.ShowModal();
}

void HelpSystem::ShowHtmlText(wxWindow *pParent,
                  const TranslatableString &Title,
                  const wxString &HtmlText,
                  bool bIsFile,
                  bool bModal)
{
   LinkingHtmlWindow *html;

   wxASSERT(pParent); // to justify safenew
   // JKC: ANSWER-ME: Why do we create a fake 'frame' and then put a BrowserDialog
   // inside it, rather than have a variant of the BrowserDialog that is a
   // frame??
   // Bug 1412 seems to be related to the extra frame.
   auto pFrame = safenew wxFrame {
      pParent, wxID_ANY, Title.Translation(), wxDefaultPosition, wxDefaultSize,
#if defined(__WXMAC__)
      // On OSX, the html frame can go behind the help dialog and if the help
      // html frame is modal, you can't get back to it.  Pressing escape gets
      // you out of this, but it's just easier to add the wxSTAY_ON_TOP flag
      // to prevent it from falling behind the dialog.  Not the perfect solution
      // but acceptable in this case.
      wxSTAY_ON_TOP |
#endif
      wxDEFAULT_FRAME_STYLE
   };

   BrowserDialog * pWnd;
   if( bModal )
      pWnd = safenew HtmlTextHelpDialog{ pFrame, Title };
   else
      pWnd = safenew BrowserDialog{ pFrame, Title };

   // Bug 1412 workaround for 'extra window'.  Hide the 'fake' window.
   pFrame->SetTransparent(0);
   ShuttleGui S( pWnd, eIsCreating );

   S.Style( wxNO_BORDER | wxTAB_TRAVERSAL )
      .Prop(true)
      .StartPanel();
   {
      S.StartHorizontalLay( wxEXPAND, false );
      {
         S.Id( wxID_BACKWARD )
            .Disable()
#if wxUSE_TOOLTIPS
            .ToolTip( XO("Backwards" ) )
#endif
            /* i18n-hint arrowhead meaning backward movement */
            .AddButton( XXO("<") );
         S.Id( wxID_FORWARD  )
            .Disable()
#if wxUSE_TOOLTIPS
            .ToolTip( XO("Forwards" ) )
#endif
            /* i18n-hint arrowhead meaning forward movement */
            .AddButton( XXO(">") );
      }
      S.EndHorizontalLay();

      html = safenew LinkingHtmlWindow(S.GetParent(), wxID_ANY,
                                   wxDefaultPosition,
                                   bIsFile ? wxSize(500, 400) : wxSize(480, 240),
                                   wxHW_SCROLLBAR_AUTO | wxSUNKEN_BORDER);

      html->SetRelatedFrame( pFrame, wxT("Help: %s") );
      if( bIsFile )
         html->LoadFile( HtmlText );
      else
         html->SetPage( HtmlText);

      S.Prop(1).Focus().Position( wxEXPAND )
         .AddWindow( html );

      S.Id( wxID_CANCEL ).AddButton( XXO("Close"), wxALIGN_CENTER, true );
   }
   S.EndPanel();

   // -- START of ICON stuff -----
   // If this section (providing an icon) causes compilation errors on linux, comment it out for now.
   // it will just mean that the icon is missing.  Works OK on Windows.
   #ifdef __WXMSW__
   wxIcon ic{ wxICON(AudacityLogo) };
   #else
   wxIcon ic{};
      ic.CopyFromBitmap(theTheme.Bitmap(bmpAudacityLogo48x48));
   #endif
   pFrame->SetIcon( ic );
   // -- END of ICON stuff -----


   pWnd->mpHtml = html;
   pWnd->SetBackgroundColour( wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE));

   pFrame->CreateStatusBar();
   pFrame->Centre();
   pFrame->Layout();
   pFrame->SetSizeHints(pWnd->GetSize());

   pFrame->SetName(Title.Translation());
   if (bModal)
      pWnd->ShowModal();
   else {
      pWnd->Show(true);
      pFrame->Show(true);
   }

   html->SetRelatedStatusBar( 0 );

   return;
}

// Shows help in browser, or possibly in own dialog.
void HelpSystem::ShowHelp(wxWindow *parent,
                    const FilePath &localFileName,
                    const URLString &remoteURL,
                    bool bModal,
                    bool alwaysDefaultBrowser)
{
   wxASSERT(parent); // to justify safenew
   wxString HelpMode = wxT("Local");

// DA: Default for DA is manual from internet.
#ifdef EXPERIMENTAL_DA
   gPrefs->Read(wxT("/GUI/Help"), &HelpMode, wxT("FromInternet") );
#else
   gPrefs->Read(wxT("/GUI/Help"), &HelpMode, wxT("Local") );
#endif

   {
      // these next lines are for legacy cfg files (pre 2.0) where we had different modes
      if( (HelpMode == wxT("Standard")) || (HelpMode == wxT("InBrowser")) )
      {
         HelpMode = GUIManualLocation.Default().Internal();
         GUIManualLocation.Write(HelpMode);
         gPrefs->Flush();
      }
   }

   // Anchors (URLs with a '#' in them) are not supported by many OSs for local file names
   // See, for example, https://groups.google.com/forum/#!topic/wx-users/pC0uOZJalRQ
   // Problems have been reported on Win, Mac and some versions of Linux.
   // So we set HelpMode to use the internet if an anchor is found.
   if (localFileName.Find('#', true) != wxNOT_FOUND)
      HelpMode = wxT("FromInternet");
   // Until a solution is found for this, the next few lines are irrelevant.

   // Obtain the local file system file name, without the anchor if present.
   wxString localfile;
   if (localFileName.Find('#', true) != wxNOT_FOUND)
      localfile = localFileName.BeforeLast('#');
   else
      localfile = localFileName;

   if( (HelpMode == wxT("FromInternet")) && !remoteURL.empty() )
   {
      // Always go to remote URL.  Use External browser.
      OpenInDefaultBrowser( remoteURL );
   }
   else if( localfile.empty() || !wxFileExists( localfile ))
   {
      // If you give an empty remote URL, you should have already ensured
      // that the file exists!
      wxASSERT( !remoteURL.empty() );
      // I can't find it'.
      // Use Built-in browser to suggest you use the remote url.
      wxString Text = HelpText( wxT("remotehelp") );
      Text.Replace( wxT("*URL*"), remoteURL.GET() );
      // Always make the 'help on the internet' dialog modal.
      // Fixes Bug 1411.
      ShowHtmlText( parent, XO("Help on the Internet"), Text, false, true );
   }
   else if( HelpMode == wxT("Local") || alwaysDefaultBrowser)
   {
      // Local file, External browser
      OpenInDefaultBrowser( L"file:" + localFileName );
   }
   else
   {
      // Local file, Built-in browser
      ShowHtmlText( parent, {}, localFileName, true, bModal );
   }
}

void HelpSystem::ShowHelp(wxWindow *parent,
                          const ManualPageID &PageName,
                          bool bModal)
{
   /// The string which is appended to the development manual page name in order
   /// obtain the file name in the local and release web copies of the manual
   const wxString ReleaseSuffix = L".html";

   FilePath localHelpPage;
   wxString webHelpPath;
   wxString webHelpPage;
   wxString releasePageName;
   wxString anchor;	// optional part of URL after (and including) the '#'
   const auto &PageNameStr = PageName.GET();
   if (PageNameStr.Find('#', true) != wxNOT_FOUND)
   {	// need to split anchor off into separate variable
      releasePageName = PageNameStr.BeforeLast('#');
      anchor = wxT("#") + PageNameStr.AfterLast('#');
   }
   else
   {
      releasePageName = PageName.GET();
      anchor = wxT("");
   }
   // The wiki pages are transformed to static HTML by
   // scripts/mw2html_audacity/mw2html.py
   // The name is first transformed to lower case, then all
   // 'special characters' are replaced by underscores. Spaces are
   // transformed to "+".
   //
   // The transformations are handled in mw2html by first applying
   // 'urllib.parse.quote_plus' (escape chars that are not in "always safe" list)
   // then replacing escape characters (%xx) with underscores,
   // and finally removing duplicate / redundant underscores.
   //
   // The front page and 'quick_help' are treated as special cases and placed in
   // the root of the help directory rather than the "/man/" sub-directory.
   if (releasePageName == L"Main_Page")
   {
      releasePageName = L"index" + ReleaseSuffix + anchor;
      localHelpPage = wxFileName(FileNames::HtmlHelpDir(), releasePageName).GetFullPath();
      webHelpPath = L"https://" + HelpSystem::HelpHostname + HelpSystem::HelpServerHomeDir;
   }
   else if (releasePageName == L"Quick_Help")
   {
// DA: No bundled help, by default, and different quick-help URL.
#ifdef EXPERIMENTAL_DA
      releasePageName = L"video" + ReleaseSuffix + anchor;
      localHelpPage = wxFileName(FileNames::HtmlHelpDir(), releasePageName).GetFullPath();
      webHelpPath = L"http://www.darkaudacity.com/";
#else
      releasePageName = L"quick_help" + ReleaseSuffix + anchor;
      localHelpPage = wxFileName(FileNames::HtmlHelpDir(), releasePageName).GetFullPath();
      webHelpPath = L"https://" + HelpSystem::HelpHostname + HelpSystem::HelpServerHomeDir;
#endif
   }
   // not a page name, but rather a full path (e.g. to wiki)
   // in which case do not do any substitutions.
   else if (releasePageName.StartsWith( "http" ) )
   {
      localHelpPage = "";
      releasePageName += anchor;
      // webHelpPath remains empty
   }
   else
   {
      // Handle all other pages.
      // Change to lower case.
      releasePageName = releasePageName.Lower();
      wxRegEx re;
      // replace 'special characters' with underscores.
      // RFC 2396 defines the characters a-z, A-Z, 0-9 and ".-_" as "always safe"
      // mw2html also replaces "-" with "_" so replace that too.
      
      // If PageName contains a %xx code, mw2html will transform it:
      // '%xx' => '%25xx' => '_'
      re.Compile(wxT("%.."));
      re.ReplaceAll(&releasePageName, (wxT("_")));
      // Now replace all other 'not-safe' characters.
      re.Compile(wxT("[^[:alnum:] . [:space:]]"));
      re.ReplaceAll(&releasePageName, (wxT("_")));
      // Replace spaces with "+"
      releasePageName.Replace(wxT(" "), wxT("+"), true);
      // Reduce multiple underscores to single underscores
      re.Compile(wxT("__+"));
      re.ReplaceAll(&releasePageName, (wxT("_")));
      // Replace "_." with "."
      releasePageName.Replace(wxT("_."), wxT("."), true);
      // Concatenate file name with file extension and anchor.
      releasePageName = releasePageName + ReleaseSuffix + anchor;
      // Other than index and quick_help, all local pages are in subdirectory 'LocalHelpManDir'.
      localHelpPage = wxFileName(FileNames::HtmlHelpDir() + LocalHelpManDir, releasePageName).GetFullPath();
      // Other than index and quick_help, all on-line pages are in subdirectory 'HelpServerManDir'.
      webHelpPath = L"https://" + HelpSystem::HelpHostname + HelpSystem::HelpServerManDir;
   }

#ifdef USE_ALPHA_MANUAL
   webHelpPage = webHelpPath + PageName.GET();
#else
   webHelpPage = webHelpPath + releasePageName;
#endif

   wxLogMessage(wxT("Help button pressed: PageName %s, releasePageName %s"),
              PageName.GET(), releasePageName);
   wxLogMessage(wxT("webHelpPage %s, localHelpPage %s"),
              webHelpPage, localHelpPage);

   wxASSERT(parent); // to justify safenew

   HelpSystem::ShowHelp(
      parent, 
      localHelpPage,
      webHelpPage,
      bModal
      );
}

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#include <wx/mimetype.h>
#include <wx/filename.h>
#include <wx/uri.h>

BEGIN_EVENT_TABLE(BrowserDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_FORWARD,  BrowserDialog::OnForward)
   EVT_BUTTON(wxID_BACKWARD, BrowserDialog::OnBackward)
   EVT_BUTTON(wxID_CANCEL,   BrowserDialog::OnClose)
   EVT_KEY_DOWN(BrowserDialog::OnKeyDown)
END_EVENT_TABLE()


BrowserDialog::BrowserDialog(wxWindow *pParent, const TranslatableString &title)
   : wxDialogWrapper{ pParent, ID, title, wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER /*| wxMAXIMIZE_BOX */  }
{
   int width, height;
   const int minWidth = 400;
   const int minHeight = 250;

   gPrefs->Read(wxT("/GUI/BrowserWidth"), &width, minWidth);
   gPrefs->Read(wxT("/GUI/BrowserHeight"), &height, minHeight);

   if (width < minWidth || width > wxSystemSettings::GetMetric(wxSYS_SCREEN_X))
      width = minWidth;
   if (height < minHeight || height > wxSystemSettings::GetMetric(wxSYS_SCREEN_Y))
      height = minHeight;

   SetMinSize(wxSize(minWidth, minHeight));
   SetSize(wxDefaultPosition.x, wxDefaultPosition.y, width, height, wxSIZE_AUTO);
}

void BrowserDialog::OnForward(wxCommandEvent & WXUNUSED(event))
{
   mpHtml->HistoryForward();
   UpdateButtons();
}

void BrowserDialog::OnBackward(wxCommandEvent & WXUNUSED(event))
{
   mpHtml->HistoryBack();
   UpdateButtons();
}

void BrowserDialog::OnClose(wxCommandEvent & WXUNUSED(event))
{
   if (IsModal() && !mDismissed)
   {
      mDismissed = true;
      EndModal(wxID_CANCEL);
   }
   auto parent = GetParent();

   gPrefs->Write(wxT("/GUI/BrowserWidth"), GetSize().GetX());
   gPrefs->Write(wxT("/GUI/BrowserHeight"), GetSize().GetY());
   gPrefs->Flush();

#ifdef __WXMAC__
   auto grandparent = GetParent()->GetParent();
#endif

   parent->Destroy();

#ifdef __WXMAC__
   if(grandparent && grandparent->IsShown()) {
      grandparent->Raise();
   }
#endif
}

void BrowserDialog::OnKeyDown(wxKeyEvent & event)
{
   bool bSkip = true;
   if (event.GetKeyCode() == WXK_ESCAPE)
   {
      bSkip = false;
      Close(false);
   }
   event.Skip(bSkip);
}


void BrowserDialog::UpdateButtons()
{
   wxWindow * pWnd;
   if( (pWnd = FindWindowById( wxID_BACKWARD, this )) != NULL )
   {
      pWnd->Enable(mpHtml->HistoryCanBack());
   }
   if( (pWnd = FindWindowById( wxID_FORWARD, this )) != NULL )
   {
      pWnd->Enable(mpHtml->HistoryCanForward());
   }
}

void OpenInDefaultBrowser(const URLString& link)
{
   wxURI uri(link.GET());
   wxLaunchDefaultBrowser(uri.BuildURI());
}

LinkingHtmlWindow::LinkingHtmlWindow(wxWindow *parent, wxWindowID id /*= -1*/,
                                       const wxPoint& pos /*= wxDefaultPosition*/,
                                       const wxSize& size /*= wxDefaultSize*/,
                                       long style /*= wxHW_SCROLLBAR_AUTO*/) :
   HtmlWindow(parent, id, pos, size, style)
{
}

void LinkingHtmlWindow::OnLinkClicked(const wxHtmlLinkInfo& link)
{
   wxString href = link.GetHref();

   if( href.StartsWith( wxT("innerlink:help:")))
   {
      HelpSystem::ShowHelp(this, ManualPageID{ href.Mid( 15 ) }, true );
      return;
   }
   else if( href.StartsWith(wxT("innerlink:")) )
   {
      wxString FileName =
         wxFileName( FileNames::HtmlHelpDir(), href.Mid( 10 ) + wxT(".htm") ).GetFullPath();
      if( wxFileExists( FileName ) )
      {
         HelpSystem::ShowHelp(this, FileName, wxEmptyString, false);
         return;
      }
      else
      {
         SetPage( HelpText( href.Mid( 10 )));
         wxGetTopLevelParent(this)->SetLabel( TitleText( href.Mid( 10 )).Translation() );
      }
   }
   else if( href.StartsWith(wxT("mailto:")) || href.StartsWith(wxT("file:")) )
   {
      OpenInDefaultBrowser( link.GetHref() );
      return;
   }
   else if( !href.StartsWith( wxT("http:"))  && !href.StartsWith( wxT("https:")) )
   {
      HtmlWindow::OnLinkClicked( link );
   }
   else
   {
      OpenInDefaultBrowser(link.GetHref());
      return;
   }
   wxFrame * pFrame = GetRelatedFrame();
   if( !pFrame )
      return;
   wxWindow * pWnd = pFrame->FindWindow(BrowserDialog::ID);
   if( !pWnd )
      return;
   BrowserDialog * pDlg = wxDynamicCast( pWnd , BrowserDialog );
   if( !pDlg )
      return;
   pDlg->UpdateButtons();
}
